(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 Bytecode loader class.
*)
{$H+}
Unit BCLoader;

 Interface
 Uses DbgParser, Stream, SysUtils, Classes, Zipper;

 Const BytecodeMajor = 0;
       BytecodeMinor = 42;

 { TLoadState }
 Type TLoadState = (lsSuccess, lsFailed, lsFileNotFound);

 { TBCLoaderData }
 Type PBCLoaderData = ^TBCLoaderData;
      TBCLoaderData =
      Record
       State: TLoadState;

       // points at first opcode
       CodeData: PByte;

       // magic number - should be equal 0x0DEFACED
       MagicNumber: uint32;

       // attributes
       isRunnable: Boolean;

       // version
       VersionMajor, VersionMinor: uint8;

       // debug data
       Debug: TDebugData;
      End;

 { TBCLoader }
 Type TBCLoader =
      Class
       Private
        ErrorMsg: String;

        FileName  : String;
        LoaderData: TBCLoaderData;

       Private
        Procedure OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
        Procedure OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

        Procedure ParseHeader(const NStream: Stream.TStream);
        Procedure ParseBytecode(const NStream: Stream.TStream);
        Procedure ParseDebug(const NStream: Stream.TStream);

       Public
        Constructor Create(const fFileName: String);

        Function Load: Boolean;

        Property getLoaderData: TBCLoaderData read LoaderData;
        Property getErrorMsg: String read ErrorMsg;
       End;

 Implementation

(* TBCLoader.OnCreateStream *)
Procedure TBCLoader.OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Begin
 AStream := Stream.TStream.Create(True);
End;

(* TBCLoader.OnDoneStream *)
Procedure TBCLoader.OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Var NStream: Stream.TStream;
Begin
 AStream.Position := 0;

 NStream := Stream.TStream(AStream);

 Try
  Case AItem.ArchiveFileName of
   '.header'  : ParseHeader(NStream);
   '.bytecode': ParseBytecode(NStream);
   '.debug'   : ParseDebug(NStream);
  End;
 Finally
  AStream.Free;
 End;
End;

(* TBCLoader.ParseHeader *)
{
 Parses header from specified stream.
}
Procedure TBCLoader.ParseHeader(const NStream: Stream.TStream);

  { EndingZero }
  Function EndingZero(const Text: String): String;
  Begin
   if (Length(Text) = 1) Then
    Exit(Text+'0') Else
    Exit(Text);
  End;

Begin
 With LoaderData do
 Begin
  MagicNumber  := NStream.read_uint32;
  isRunnable   := Boolean(NStream.read_uint8);
  VersionMajor := NStream.read_uint8;
  VersionMinor := NStream.read_uint8;

  // check magic number
  if (MagicNumber <> $0DEFACED) Then
  Begin
   ErrorMsg := Format('Invalid magic number: 0x%x', [MagicNumber]);
   Exit;
  End;

  // check bytecode version
  if (VersionMajor <> BytecodeMajor) or (VersionMinor <> BytecodeMinor) Then
  Begin
   ErrorMsg := Format('Unsupported bytecode version: %d.%s, expecting %d.%s', [VersionMajor, EndingZero(IntToStr(VersionMinor)),
                                                                               BytecodeMajor, EndingZero(IntToStr(BytecodeMinor))]);
   Exit;
  End;
 End;
End;

(* TBCLoader.ParseBytecode *)
{
 Parses bytecode from specified stream.
}
Procedure TBCLoader.ParseBytecode(const NStream: Stream.TStream);
Var I: uint32;
Begin
 With LoaderData do
 Begin
  if (NStream.Size = 0) Then
  Begin
   ErrorMsg := 'No bytecode to be loaded!';
   Exit;
  End;

  CodeData := AllocMem(NStream.Size);
  For I := 0 To NStream.Size-1 Do // @TODO: NStream.Read/NStream.ReadBuffer
   CodeData[I] := NStream.ReadByte;
 End;
End;

(* TBCLoader.ParseDebug *)
{
 Parses debug data from specified stream.
}
Procedure TBCLoader.ParseDebug(const NStream: Stream.TStream);
Var Debug: TDebugDataReader;
Begin
 Debug := TDebugDataReader.Create(NStream);

 Try
  Try
   LoaderData.Debug := Debug.Read;
  Except
   On E: Exception Do
    ErrorMsg := E.Message;
  End;
 Finally
  Debug.Free;
 End;
End;

(* TBCLoader.Create *)
Constructor TBCLoader.Create(const fFileName: String);
Begin
 FileName := fFileName;
 ErrorMsg := '';
End;

(* TBCLoader.Load *)
{
 Loads bytecode from specified file.
}
Function TBCLoader.Load: Boolean;
Var Zip     : TUnzipper;
    FileList: TStringList;

  { UnzipAndParse }
  Function UnzipAndParse(const FileName: String): Boolean; inline;
  Begin
   FileList.Clear;
   FileList.Add(FileName);
   Zip.UnzipFiles(FileList);

   Result := (Length(ErrorMsg) = 0);
  End;

Begin
 if (not FileExists(FileName)) Then // file doesn't exist
 Begin
  LoaderData.State := lsFileNotFound;
  Exit(False);
 End;

 Zip          := TUnzipper.Create;
 Zip.FileName := FileName;

 FileList := TStringList.Create;

 LoaderData.State := lsSuccess;

 Try
  LoaderData.CodeData := nil;

  Zip.Examine;
  Zip.OnCreateStream := @OnCreateStream;
  Zip.OnDoneStream   := @OnDoneStream;

  Result := UnzipAndParse('.header') and UnzipAndParse('.bytecode') and UnzipAndParse('.debug');

  if (not Result) Then
  Begin
   LoaderData.State := lsFailed;
   Exit(False);
  End;
 Finally
  FileList.Free;
  Zip.Free;
 End;

 Exit(True);
End;
End.
