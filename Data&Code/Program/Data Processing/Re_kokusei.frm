VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3090
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3090
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows �̊���l
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   735
      Left            =   960
      TabIndex        =   0
      Top             =   1080
      Width           =   1455
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim data(100000) As String
Dim temp As String
Dim x As Integer
Dim y As Long
Dim myfile, mypath, myname
Dim pps As String

'* �ړI�@���������f�[�^�𐮗�
'�@���������̃t�@�C������̃t�H���_�ɓ����B
'�A�ǌ�����ŁA�����ď����o���B

Private Sub Command1_Click()

y = 0 '�uy�v�������݃��R�[�h�ԍ�
z = 1 '�uz�v�t�@�C���ԍ�

'���������t�@�C���������Ă����΃p�X
Open "C:\Documents and Settings\Owner\�f�X�N�g�b�v\DC1-13.TXT" For Input As #1

'�����o���t�@�C����
Open "Re_Kokusei.txt" For Output As #2

'�ǂݍ���
Do Until y = 100
    Line Input #1, temp
    data(y) = temp
    y = y + 1
Loop

'�����o��
For i = 0 To y
    Write #2, data(i)
Next i

MsgBox "�v�Z�I���"

End Sub

