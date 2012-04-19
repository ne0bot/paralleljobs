object frmMain: TfrmMain
  Left = 302
  Top = 219
  Width = 579
  Height = 619
  Caption = 
    'Client Socket Communication :: http://code.google.com/p/parallel' +
    'jobs/'
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
  DesignSize = (
    563
    581)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 16
    Width = 25
    Height = 13
    Caption = 'Host:'
  end
  object Label3: TLabel
    Left = 144
    Top = 16
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object Label4: TLabel
    Left = 240
    Top = 16
    Width = 65
    Height = 13
    Caption = 'Clients Count:'
  end
  object Label5: TLabel
    Left = 16
    Top = 64
    Width = 78
    Height = 13
    Caption = 'Test commands:'
  end
  object edtHostAddr: TEdit
    Left = 16
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object edtHostPort: TEdit
    Left = 144
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '2636'
  end
  object edtClientsCount: TEdit
    Left = 240
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 3
    Text = '1500'
  end
  object btnActiveClients: TButton
    Left = 328
    Top = 22
    Width = 97
    Height = 25
    Caption = 'Active Clients'
    TabOrder = 0
    OnClick = btnActiveClientsClick
  end
  object mmoLog: TMemo
    Left = 8
    Top = 104
    Width = 544
    Height = 473
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
  end
  object cbbClientCmds: TComboBox
    Left = 16
    Top = 80
    Width = 449
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'Send random number'
    Items.Strings = (
      'Send random number'
      'Send command and wait random response'
      'Send command and request server time'
      'Send command and request clients count')
  end
  object btnClientSendCmd: TButton
    Left = 472
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Send Cmd'
    TabOrder = 4
    OnClick = btnClientSendCmdClick
  end
end
