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
   StartUpPosition =   3  'Windows の既定値
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

'* 目的　国勢調査データを整理
'①国勢調査のファイルを一つのフォルダに入れる。
'②読見込んで、そして書き出す。

Private Sub Command1_Click()

y = 0 '「y」書き込みレコード番号
z = 1 '「z」ファイル番号

'国勢調査ファイルが入っている絶対パス
Open "C:\Documents and Settings\Owner\デスクトップ\DC1-13.TXT" For Input As #1

'書き出すファイル名
Open "Re_Kokusei.txt" For Output As #2

'読み込み
Do Until y = 100
    Line Input #1, temp
    data(y) = temp
    y = y + 1
Loop

'書き出し
For i = 0 To y
    Write #2, data(i)
Next i

MsgBox "計算終わり"

End Sub

