{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    This unit forms part of the PDF Reporting Engine. This unit
    contains the images usde by the report preview form.

    The PDF Reporting Engine was originally written by
    Jean-Marc Levecque <jean-marc.levecque@jmlesite.fr>
}

unit U_ReportImages;

{$mode objfpc}{$H+}

interface

uses
  fpg_main;

procedure CreateReportImages;
function DeleteReportImages: Boolean;

implementation

const
    repimg_Last: array[0..821] of byte = (
        66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
         0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
         0,  3,  0,  0, 18, 11,  0,  0, 18, 11,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,  0,  0,128,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
         0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,
       255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,
         0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,128,  0,
         0,128,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,
         0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,128,  0,  0,128,  0,
         0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,  0,  0,128,
         0,  0,128,255,  0,255,255,  0,255,128,  0,  0,128,  0,  0,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,
       255,  0,255,255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,
         0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255);

const
    repimg_Printer: array[0..1253] of byte = (
        66, 77,230,  4,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
         0, 20,  0,  0,  0, 20,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
       176,  4,  0,  0, 19, 11,  0,  0, 19, 11,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,  0,  0,  0,  0,  0,  0,  0,
         0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,
         0,  0,  0,  0,  0,  0,  0,255,  0,255,255,  0,255,255,  0,255,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,255,  0,255,255,  0,255,  0,  0,  0,255,255,255,192,192,192,
       192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,
       192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,
       192,192,192,192,192,192,192,192,  0,  0,  0,255,  0,255,255,  0,255,
         0,  0,  0,255,255,255,192,192,192,192,192,192,192,192,192,192,192,
       192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,
       192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,
         0,  0,  0,255,  0,255,255,  0,255,  0,  0,  0,255,255,255,192,192,
       192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,192,
       192,192,192,192,192,192,192,192,192,192,192,192,192,192,  0,  0,255,
         0,  0,255,192,192,192,192,192,192,  0,  0,  0,255,  0,255,255,  0,
       255,  0,  0,  0,255,255,255,255,255,255,255,255,255,255,255,255,255,
       255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
       255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
       255,  0,  0,  0,255,  0,255,255,  0,255,128,128,128,  0,  0,  0,  0,
         0,  0,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
       128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
       128,128,128,128,  0,  0,  0,  0,  0,  0,128,128,128,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
         0,  0,  0,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
       128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
       128,128,128,128,128,  0,  0,  0,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,128,128,128,  0,  0,  0,  0,  0,  0,  0,  0,
         0,128,128,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,
         0,255,255,  0,255,255,255,255,255,  0,  0,  0,  0,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,128,128,128,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,255,255,  0,255,255,255,  0,  0,  0,  0,
         0,  0,128,128,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,  0,  0,  0,255,255,  0,  0,  0,  0,255,255,  0,255,255,
       255,255,255,  0,255,255,255,255,255,  0,255,255,255,255,255,  0,255,
       255,255,255,255,  0,  0,  0,  0,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,128,128,128,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,255,255,  0,255,255,255,  0,  0,  0,
         0,  0,  0,128,128,128,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
         0,  0,  0,255,255,  0,255,255,255,255,255,  0,  0,  0,  0,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,128,128,128,  0,  0,  0,  0,  0,
         0,  0,  0,  0,128,128,128,255,  0,255,255,  0,255);

const
    repimg_Previous: array[0..821] of byte = (
        66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
         0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
         0,  3,  0,  0, 18, 11,  0,  0, 18, 11,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,
       128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
         0,  0,128,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,
         0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,
         0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,
         0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255);

const
    repimg_Stop: array[0..821] of byte = (
        66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
         0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
         0,  3,  0,  0, 19, 11,  0,  0, 19, 11,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,
       255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,  0,  0,255,  0,  0,255,  0,  0,
       255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,
       255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,255,
         0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,
       255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,255,  0,255,  0,  0,255,  0,  0,255,255,255,255,
       255,255,255,  0,  0,255,  0,  0,255,255,255,255,  0,  0,255,  0,  0,
       255,255,255,255,  0,  0,255,  0,  0,255,255,255,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,255,255,255,  0,  0,255,  0,  0,255,
       255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,
       255,255,255,255,  0,  0,255,255,255,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,255,255,255,
         0,  0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,
       255,  0,  0,255,255,255,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,255,255,255,  0,  0,255,  0,  0,255,
       255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,
       255,255,255,255,255,255,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,255,255,255,  0,  0,255,  0,  0,255,  0,  0,255,255,255,255,
         0,  0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,
       255,  0,  0,255,255,255,255,  0,  0,255,  0,  0,255,255,255,255,  0,
         0,255,  0,  0,255,  0,  0,255,  0,  0,255,255,255,255,  0,  0,255,
       255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,
       255,255,255,255,  0,  0,255,  0,  0,255,255,255,255,  0,  0,255,  0,
         0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,255,
         0,  0,255,255,255,255,  0,  0,255,255,255,255,  0,  0,255,255,255,
       255,  0,  0,255,  0,  0,255,  0,  0,255,255,255,255,255,255,255,  0,
         0,255,255,255,255,255,255,255,255,255,255,  0,  0,255,255,255,255,
         0,  0,255,  0,  0,255,255,255,255,255,255,255,  0,  0,255,  0,  0,
       255,255,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,
         0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,255,  0,255,255,  0,
       255,255,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,
         0,  0,255,  0,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,  0,255,  0,
         0,255,  0,  0,255,  0,  0,255,  0,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255);

const
    repimg_Next: array[0..821] of byte = (
        66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
         0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
         0,  3,  0,  0, 18, 11,  0,  0, 18, 11,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,  0,  0,128,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
         0,  0,128,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,
         0,  0,128,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,
         0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,  0,  0,128,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
       255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
         0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       255,  0,255,255,  0,255);


const
  repimg_First: array[0..821] of byte = (
      66, 77, 54,  3,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0, 16,  0,  0,  0, 16,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
       0,  3,  0,  0, 18, 11,  0,  0, 18, 11,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,  0,  0,128,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,128,  0,  0,
     128,  0,  0,255,  0,255,255,  0,255,255,  0,255,  0,  0,128,  0,  0,
     128,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,128,  0,  0,128,  0,  0,
     255,  0,255,255,  0,255,  0,  0,128,  0,  0,128,  0,  0,128,  0,  0,
     128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,
       0,  0,128,  0,  0,128,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,
       0,  0,128,  0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,128,  0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,255,
       0,  0,128,  0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,128,
       0,  0,128,  0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       0,  0,128,  0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,128,  0,  0,128,
       0,  0,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
       0,  0,128,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,
     255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,
       0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,255,  0,255,
     255,  0,255,255,  0,255);


procedure CreateReportImages;
begin

  fpgImages.AddMaskedBMP('repimg.Last',@repimg_Last,sizeof(repimg_Last),0,0);

  fpgImages.AddMaskedBMP('repimg.Printer',@repimg_Printer,sizeof(repimg_Printer),0,0);

  fpgImages.AddMaskedBMP('repimg.Previous',@repimg_Previous,sizeof(repimg_Previous),0,0);

  fpgImages.AddMaskedBMP('repimg.Stop',@repimg_Stop,sizeof(repimg_Stop),0,0);

  fpgImages.AddMaskedBMP('repimg.Next',@repimg_Next,sizeof(repimg_Next),0,0);

  fpgImages.AddMaskedBMP('repimg.First',@repimg_First,sizeof(repimg_First),0,0);

end;

function DeleteReportImages: Boolean;
begin

  fpgImages.DeleteImage('repimg.Last',True);

  fpgImages.DeleteImage('repimg.Printer',True);

  fpgImages.DeleteImage('repimg.Previous',True);

  fpgImages.DeleteImage('repimg.Stop',True);

  fpgImages.DeleteImage('repimg.Next',True);

  fpgImages.DeleteImage('repimg.First',True);

end;

end.

