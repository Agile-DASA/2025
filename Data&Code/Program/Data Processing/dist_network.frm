VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   8580
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   12885
   LinkTopic       =   "Form1"
   ScaleHeight     =   8580
   ScaleWidth      =   12885
   StartUpPosition =   3  'Windows Μωθl
   Visible         =   0   'False
   Begin VB.CommandButton Command3 
      Caption         =   "ListCaps"
      Height          =   615
      Left            =   6360
      TabIndex        =   1
      Top             =   6360
      Width           =   1575
   End
   Begin VB.CommandButton Command1 
      Caption         =   "program(&Q)|=point"
      Height          =   615
      Left            =   2040
      TabIndex        =   0
      Top             =   6360
      Width           =   1815
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'------------------------------------sζ¬ΊΤΜlbg[N£πWv·ιvO----------------------------------
Const nMax = 3000
'bV
Dim MNum As Integer         '-sζ¬ΊΜ
Dim x_m(nMax) As Double     '-sζ¬ΊΜΐW
Dim y_m(nMax) As Double
Dim MDId(nMax) As Double    '-sζ¬ΊΜlϋ
Dim MMId(nMax) As Integer   '-sζ¬ΊΜID
'«o΅pA£f[^
Dim DList(nMax, nMax) As Double 'ͺθ΅½£Μzρ

'κΟ
Dim temp As Double
Dim temp1 As Double
Dim temp2 As Integer
Dim temp3 As Double
Dim temp4 As Double
Dim temp5 As Integer
Dim temp6 As Double
Dim temp7 As Double
Dim temp8 As Integer
Dim temp9 As Integer
Dim o As Double
Dim SNumb1 As Integer
Dim SNumb2 As Integer
Private Sub Command1_Click()

'sζ¬ΊC[ΜΗέέ-------------------
'sζ¬ΊC[u0v
MNum = GisScanOverlay("sζ¬Ί", 0, "", "")
For i = 0 To MNum - 1
        MMId(i) = GisGetListItemInt("sζ¬Ί", Str(i), "_id&")   'ID
        x_m(MMId(i)) = GisGetListItemFlt("sζ¬Ί", Str(i), "_ox#")  'ΐWx
        y_m(MMId(i)) = GisGetListItemFlt("sζ¬Ί", Str(i), "_oy#")  'ΐWy
        GisMessage ("bVf[^ Ηέέ" + Str(i) + "/" + Str(MNum))
Next i

GisSetInt SIS_OT_OVERLAY, 1, "_status&", SIS_HITTABLE
    
'lbg[N£ͺθ
For i = 0 To MNum - 1
    For j = i + 1 To MNum - 1
    DList(MMId(i), MMId(j)) = ((x_m(MMId(i)) - x_m(MMId(j))) ^ 2 + (y_m(MMId(i)) - y_m(MMId(j))) ^ 2) ^ 0.5
    GisMessage ("£vͺ" + Str(i) + "/" + Str(j) + "/" + Str(MNum))
    Next j
Next i
For i = 0 To MNum - 1
    For j = i + 1 To MNum - 1
        DList(MMId(j), MMId(i)) = DList(MMId(i), MMId(j))
    Next j
Next i
For i = 1 To MNum
        DList(i, i) = 0
Next i

'f[^«o΅
Open "DList.csv" For Output As #1
    For i = 1 To MNum
        For j = 1 To MNum
        Write #1, DList(i, j);
        Next j
        Write #1,
    Next i
Close #1

MsgBox "vZIνθ"
GisRelease

End Sub

Private Sub Command3_Click()
    GisAddCommand "program(&Q)|=point", "", "Item", 0, -1, "", ""
    GisRelease
End Sub
Private Sub Form_Load()
    If GisSetupLink(hwnd) = False Then
        MsgBox "LinkΙΈs"
        Exit Sub
    End If
End Sub


