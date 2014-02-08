(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 -------------------
 Bytecode loader class.
*)
{$H+}
Unit BCLoader;

 Interface
 Uses SysUtils, Classes, Zipper;

 Const BytecodeMajor = 0;
       BytecodeMinor = 42;

 { TLoadState }
 Type TLoadState = (lsSuccess, lsFailed, lsFileNotFound);

 { TBCLoaderData }
 Type PBCLoaderData = ^TBCLoaderData;
      TBCLoaderData =
      Record
       State: TLoadState;

       CodeData: PByte;

       MagicNumber               : uint32;
       isRunnable                : Boolean;
       VersionMajor, VersionMinor: uint8;
      End;

 { TBCLoader }
 Type TBCLoader =
      Class
       Private
        ErrorMsg: String;

        FileName  : String;
        LoaderData: PBCLoaderData;

       Private
        Procedure OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
        Procedure OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

        Procedure ParseHeader(AStream: TStream);
        Procedure ParseBytecode(AStream: TStream);

       Public
        Constructor Create(const fFileName: String);

        Function Load: Boolean;

        Property getLoaderData: PBCLoaderData read LoaderData;
        Property getErrorMsg: String read ErrorMsg;
       End;

 Implementation

(* TBCLoader.OnCreateStream *)
Procedure TBCLoader.OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Begin
 AStream := TMemoryStream.Create;
End;

(* TBCLoader.OnDoneStream *)
Procedure TBCLoader.OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
Begin
 AStream.Position := 0;

 Try
  Case AItem.ArchiveFileName of
   '.header'  : ParseHeader(AStream);
   '.bytecode': ParseBytecode(AStream);
  End;
 Finally
  AStream.Free;
 End;
End;

(* TBCLoader.ParseHeader *)
{
 Parses header from specified stream.
}
Procedure TBCLoader.ParseHeader(AStream: TStream);

  { EndingZero }
  Function EndingZero(const Text: String): String;
  Begin
   if (Length(Text) = 1) Then
    Exit(Text+'0') Else
    Exit(Text);
  End;

Begin
 With LoaderData^ do
 Begin
  MagicNumber  := BEtoN(AStream.ReadDWord);
  isRunnable   := Boolean(AStream.ReadByte);
  VersionMajor := AStream.ReadByte;
  VersionMinor := AStream.ReadByte;

  if (MagicNumber <> $0DEFACED) Then // error: invalid magic number
  Begin
   ErrorMsg := Format('Invalid magic number: %i', [MagicNumber]);
   Exit;
  End;

  if (VersionMajor <> BytecodeMajor) or (VersionMinor <> BytecodeMinor) Then // error: invalid bytecode version
  Begin
   ErrorMsg := Format('Unsupported bytecode version: %s.%s, expecting %s.%s', [IntToStr(VersionMajor), EndingZero(IntToStr(VersionMinor)),
                                                                               IntToStr(BytecodeMajor), EndingZero(IntToStr(BytecodeMinor))]);
   Exit;
  End;
 End;
End;

(* TBCLoader.ParseBytecode *)
{
 Parses bytecode from specified stream.
}
Procedure TBCLoader.ParseBytecode(AStream: TStream);
Var I: uint32;
Begin
 With LoaderData^ do
 Begin
  if (AStream.Size = 0) Then
  Begin
   ErrorMsg := 'No bytecode to be loaded!';
   Exit;
  End;

  CodeData := AllocMem(AStream.Size);
  For I := 0 To AStream.Size-1 Do // @TODO: AStream.Read/AStream.ReadBuffer
   CodeData[I] := AStream.ReadByte;
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

   if (Length(ErrorMsg) > 0) Then
    Exit(False) Else
    Exit(True);
  End;

Begin
 New(LoaderData);

 if (not FileExists(FileName)) Then // file doesn't exist
 Begin
  LoaderData^.State := lsFileNotFound;
  Exit(False);
 End;

 Zip          := TUnzipper.Create;
 Zip.FileName := FileName;

 FileList := TStringList.Create;

 LoaderData^.State := lsSuccess;

 Try
  LoaderData^.CodeData := nil;

  Zip.Examine;
  Zip.OnCreateStream := @OnCreateStream;
  Zip.OnDoneStream   := @OnDoneStream;

  Result := UnzipAndParse('.header') and UnzipAndParse('.bytecode');

  if (not Result) Then
  Begin
   LoaderData^.State := lsFailed;
   Exit(False);
  End;
 Finally
  FileList.Free;
  Zip.Free;
 End;

 Exit(True);
End;
End.
