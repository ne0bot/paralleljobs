object frmMain: TfrmMain
  Left = 433
  Top = 231
  Width = 328
  Height = 277
  Caption = 'Bouncing Ball - ParallelJobs'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 30
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
  object pnlDisplay: TPanel
    Left = 0
    Top = 30
    Width = 320
    Height = 220
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 1
  end
end
