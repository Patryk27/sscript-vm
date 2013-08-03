(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 Bytecode loader class for version 0.41.
*)
{$H+}
Unit BCLoader;

 Interface
 Uses SysUtils, Classes, Zipper;

 Const BytecodeMajor = 0;
       BytecodeMinor = 41;

 { TLoadState }
 Type TLoadState = (lsSuccess, lsFailed, lsFileNotFound);

 { TBCLoaderData }
 Type PBCLoaderData = ^TBCLoaderData;
      TBCLoaderData = Record
                       State: TLoadState;

                       CodeData: PByte;

                       MagicNumber               : uint32;
                       isRunnable                : Boolean;
                       VersionMajor, VersionMinor: uint8;
                      End;

 { TBCLoader }
 Type TBCLoader = Class
                   Private
                    FileName  : String;
                    LoaderData: PBCLoaderData;

                    Procedure OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
                    Procedure OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

                    Procedure ParseHeader(AStream: TStream);
                    Procedure ParseBytecode(AStream: TStream);

                   Public
                    Constructor Create(const fFileName: String);
                    Function Load: PBCLoaderData;
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

  if (MagicNumber <> $0DEFACED) Then
   raise Exception.CreateFmt('Invalid magic number: %i', [MagicNumber]);

  if (VersionMajor <> BytecodeMajor) or (VersionMinor <> BytecodeMinor) Then
   raise Exception.CreateFmt('Unsupported bytecode version: %s.%s, expecting %s.%s', [IntToStr(VersionMajor), EndingZero(IntToStr(VersionMinor)),
                                                                                      IntToStr(BytecodeMajor), EndingZero(IntToStr(BytecodeMinor))]);
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
   raise Exception.Create('No bytecode to be loaded!');

  CodeData := AllocMem(AStream.Size);
  For I := 0 To AStream.Size-1 Do // @TODO: AStream.Read/AStream.ReadBuffer
   CodeData[I] := AStream.ReadByte;
 End;
End;

// -------------------------------------------------------------------------- //
(* TBCLoader.Create *)
Constructor TBCLoader.Create(const fFileName: String);
Begin
 FileName := fFileName;
End;

(* TBCLoader.Load *)
{
 Loads bytecode from specified file.
}
Function TBCLoader.Load: PBCLoaderData;
Var Zip     : TUnzipper;
    FileList: TStringList;
Begin
 New(Result);
 LoaderData := Result;

 if (not FileExists(FileName)) Then // file doesn't exist
 Begin
  Result^.State := lsFileNotFound;
  Exit;
 End;

 Zip          := TUnzipper.Create;
 Zip.FileName := FileName;

 FileList := TStringList.Create;

 Result^.State := lsSuccess;

 Try
  Try
   Result^.CodeData := nil;

   Zip.Examine;
   Zip.OnCreateStream := @OnCreateStream;
   Zip.OnDoneStream   := @OnDoneStream;

   { unzip and parse files }
   FileList.Clear;
   FileList.Add('.header');
   Zip.UnzipFiles(FileList);

   FileList.Clear;
   FileList.Add('.bytecode');
   Zip.UnzipFiles(FileList);
  Finally
   FileList.Free;
   Zip.Free;
  End;
 Except
  Result^.State := lsFailed;
  Exit;
 End;
End;
End.
