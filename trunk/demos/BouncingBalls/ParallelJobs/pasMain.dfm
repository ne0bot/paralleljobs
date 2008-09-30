object frmMain: TfrmMain
  Left = 433
  Top = 231
  BorderStyle = bsDialog
  Caption = 'Bouncing Ball - ParallelJobs'
  ClientHeight = 250
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInfo: TPanel
    Left = 0
    Top = 220
    Width = 320
    Height = 30
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object lblInfo: TLabel
      Left = 16
      Top = 8
      Width = 9
      Height = 13
      Caption = '...'
    end
    object btnSwitchDMode: TSpeedButton
      Left = 238
      Top = 8
      Width = 19
      Height = 16
      Caption = ';'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Webdings'
      Font.Style = []
      ParentFont = False
      OnClick = btnSwitchDModeClick
    end
    object btnAddBall: TSpeedButton
      Left = 216
      Top = 8
      Width = 19
      Height = 16
      Caption = '+'
      OnClick = btnAddBallClick
    end
    object btnClean: TSpeedButton
      Left = 264
      Top = 8
      Width = 48
      Height = 16
      Caption = 'Clean'
      OnClick = btnCleanClick
    end
  end
end
