object frmMain: TfrmMain
  Left = 433
  Top = 231
  BorderStyle = bsDialog
  Caption = 
    'Bouncing Balls - ParallelJobs :: http://code.google.com/p/parall' +
    'eljobs/'
  ClientHeight = 246
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInfo: TPanel
    Left = 0
    Top = 216
    Width = 566
    Height = 30
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      566
      30)
    object lblInfo: TLabel
      Left = 16
      Top = 8
      Width = 9
      Height = 13
      Caption = '...'
    end
    object btnSwitchDMode: TSpeedButton
      Left = 484
      Top = 8
      Width = 19
      Height = 16
      Anchors = [akTop, akRight]
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
      Left = 462
      Top = 8
      Width = 19
      Height = 16
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = btnAddBallClick
    end
    object btnClean: TSpeedButton
      Left = 510
      Top = 8
      Width = 48
      Height = 16
      Anchors = [akTop, akRight]
      Caption = 'Clean'
      OnClick = btnCleanClick
    end
    object btnAdd100: TButton
      Left = 410
      Top = 8
      Width = 49
      Height = 16
      Anchors = [akTop, akRight]
      Caption = 'Add 100'
      TabOrder = 0
      OnClick = btnAdd100Click
    end
  end
end
