(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 -------------------
 Bytecode loader class.
*)
{$H+}
Unit BCLoader;

 Interface
 Uses SysUtils, Classes, Zipper, VM;

 Type TBCLoader = Class
                   Private
                    VM      : PVM;
                    FileName: PChar;

                    Procedure OnCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
                    Procedure OnDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

                    Procedure ParseHeader(AStream: TStream);
                    Procedure ParseBytecode(AStream: TStream);

                   Public
                    Constructor Create(fVM: PVM; fFileName: PChar);
                    Procedure Load;
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

    // EndingZero
    Function EndingZero(const Text: String): String;
    Begin
     if (Length(Text) = 1) Then
      Exit(Text+'0') Else
      Exit(Text);
    End;

Begin
 With VM^.Loader do
 Begin
  MagicNumber  := BEtoN(AStream.ReadDWord);
  isRunnable   := Boolean(AStream.ReadByte);
  VersionMajor := AStream.ReadByte;
  VersionMinor := AStream.ReadByte;

  if (MagicNumber <> $0DEFACED) Then
   raise Exception.CreateFmt('Invalid magic number: %i', [MagicNumber]);

  if (VersionMajor <> SupportedBytecode.Major) or (VersionMinor <> SupportedBytecode.Minor) Then
   raise Exception.CreateFmt('Unsupported bytecode version: %s.%s, expecting %s.%s', [IntToStr(VersionMajor), EndingZero(IntToStr(VersionMinor)),
                                                                                      IntToStr(SupportedBytecode.Major), EndingZero(IntToStr(SupportedBytecode.Minor))]);
 End;
End;

(* TBCLoader.ParseBytecode *)
{
 Parses bytecode from specified stream.
}
Procedure TBCLoader.ParseBytecode(AStream: TStream);
Var I: uint32;
Begin
 With VM^ do
 Begin
  if (AStream.Size = 0) Then
   raise Exception.Create('No bytecode to be loaded!');

  CodeData := AllocMem(AStream.Size);

  For I := 0 To AStream.Size-1 Do // @TODO: AStream.Read
   CodeData[I] := AStream.ReadByte;
 End;
End;

// -------------------------------------------------------------------------- //
(* TBCLoader.Create *)
Constructor TBCLoader.Create(fVM: PVM; fFileName: PChar);
Begin
 VM       := fVM;
 FileName := fFileName;
End;

(* TBCLoader.Load *)
{
 Loads bytecode from specified file.
}
Procedure TBCLoader.Load;
Var Zip     : TUnzipper;
    FileList: TStringList;
Begin
 if (not FileExists(FileName)) Then // file not exists
  Exit;

 Zip      := TUnzipper.Create;
 FileList := TStringList.Create;

 Zip.FileName := FileName;

 With VM^ do
 Begin
  Try
   CodeData := nil;

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
 End;
End;
End.
