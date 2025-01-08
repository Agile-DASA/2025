VERSION 5.00
Begin VB.Form frmDela 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  '固定(実線)
   Caption         =   "frmDela1"
   ClientHeight    =   3435
   ClientLeft      =   45
   ClientTop       =   360
   ClientWidth     =   2400
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   229
   ScaleMode       =   3  'ﾋﾟｸｾﾙ
   ScaleWidth      =   160
   StartUpPosition =   3  'Windows の既定値
   Begin VB.CommandButton Command2 
      Caption         =   "ListCaps"
      Height          =   375
      Left            =   480
      TabIndex        =   1
      Top             =   480
      Width           =   1815
   End
   Begin VB.CommandButton Command1 
      Caption         =   "program(&Q)|=Doronay"
      Height          =   375
      Left            =   360
      TabIndex        =   0
      Top             =   1080
      Width           =   1935
   End
End
Attribute VB_Name = "frmDela"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
    '-------------------------------------ドローネ三角形作成プログラム-------------------------------------------------
    Const nMax = 500
    Const mMax = 500
    Const lMax = 3000
    Dim NNN, NP As Integer
    Dim MMM As Integer
    Dim id(nMax) As Integer
    'ax,ayランダムポイント　ad(ポイントID)
    Dim ax(nMax), ay(nMax), ad(nMax) As Variant
    Dim ax2(nMax), ay2(nMax) As Double
    Dim kx(lMax), ky(lMax) As Variant
    Dim make As Integer
    Dim break3, break, break2, u, k2 As Integer
    Dim ret As Long
    Dim i, j, k, n, m, p As Integer
    Dim l As Integer
    Dim q1 As Long
    Dim list(nMax, nMax) As Double
    Dim DList(nMax, nMax) As Double
    Dim KList(105) As Integer
    Dim temp, temp2, temp3, temp4 As Integer
    Dim tempH(nMax) As Integer
    Dim tempH2(nMax) As Double
    Dim Dike(nMax, 3)
    Dim tttmmm#
    Dim ccccc%

    Private Type Toshi
    'ノードを宣言
    x As Double
    y As Double
    End Type
    Dim temp6, temp7, temp8, temp9 As Double
    Dim MinX, MinY, MaxX, MaxY, xLine, yLine As Double
    
Private Sub Command1_Click()
    Dim di, dij, cijy, cijx, sij, tij As Double
    Dim ijxs, ijys, xx, yy, ijxe, ijye, yy2 As Double
    Dim di2, dik, ciky, cikx, sik, tik, ds, us As Double
    Dim ikxs, ikxe, sibunten1, sibunten2, saijx, saikx, saijy, saiky As Double
    make = 0
    
    '市区町村レイヤーの読み込み
    NNN = GisScanOverlay("DA", 0, "", "")       'レイヤー＝「0」
    'ポイント数
    temp6 = 10 ^ 8
    temp7 = 10 ^ 8

    For i = 0 To NNN - 1
        id(i) = GisGetListItemInt("DA", Str(i), "_id&")
        ax(i) = GisGetListItemFlt("DA", Str(i), "_ox#")
        ay(i) = GisGetListItemFlt("DA", Str(i), "_oy#")
        ax2(i) = GisGetListItemFlt("DA", Str(i), "_oLat#")
        ay2(i) = GisGetListItemFlt("DA", Str(i), "_oLon#")
        GisMessage ("DP 読み込み中" + Str(i) + "/" + Str(NNN))
        If (temp6 > ax(i)) Then
        temp6 = ax(i)
        End If
        If (temp7 > ay(i)) Then
        temp7 = ay(i)
        End If
    Next i
    '
    MinX = temp6    'xの最小値
    MinY = temp7    'yの最小値
    '
    '基準化------
    For i = 0 To NNN - 1
        ax(i) = ax(i) - MinX
        ay(i) = ay(i) - MinY
    Next i
    '
    temp8 = -(10 ^ 8)
    temp9 = -(10 ^ 8)
    For i = 0 To NNN - 1
        If (ax(i) > temp8) Then
        temp8 = ax(i)
        End If
        If (ay(i) > temp9) Then
        temp9 = ay(i)
        End If
    Next i
    MaxX = temp8    'xの最大値
    MaxY = temp9    'yの最大値
    
    temp6 = 0
    temp7 = 0
    temp8 = 0
    temp9 = 0
    ijxe = 0
    ijye = 0
    
    For i = 0 To NNN - 1
        For j = 0 To NNN - 1
             list(i, j) = 10 ^ 8
        Next j
    Next i
    
    'point[all NNN+2個]
    'ポイントiに対して、ドローネを作成
    For i = 0 To NNN - 2
        'iに対して、jがひけるか
        For j = i + 1 To NNN - 1
            'i,jの傾き[di]
            di = (ay(i) - ay(j)) / (ax(i) - ax(j))
            'i,jの垂直二等分線の傾き[dij]
            dij = -1 / di
            'i,jの中点
            cijy = (ay(i) + ay(j)) / 2
            cijx = (ax(i) + ax(j)) / 2
            'i,jの切片
            sij = cijy - cijx * dij
            'i,j距離
            tij = Dist(ax(i), ay(i), ax(j), ay(j))
            '//[ijxs],[ijys]垂直二等分線の始点
            If sij > 0 And sij < MaxY Then '切片が画面内ならば
                ijxs = 0
                ijys = sij
            Else '画面外ならば
                If dij > 0 Then  '垂直二等分線の傾きが＋ならば
                    ijxs = -sij / dij
                    ijys = 0
                Else        '垂直二等分線の傾きが－ならば
                    ijxs = (MaxY - sij) / dij
                    ijys = MaxY
                End If
            End If
            '//[ijxe],[ijye]垂直二等分線の終点
            yy = dij * MaxX + sij
            If yy > 0 And yy < MaxY Then '切片が画面内ならば
                ijxe = MaxX
                ijye = yy
            Else  '画面外ならば
                If dij > 0 Then   '垂直二等分線の傾きが＋ならば
                    ijxe = (MaxY - sij) / dij
                    ijye = MaxY
                Else  '垂直二等分線の傾きが－ならば
                    ijxe = -sij / dij
                    ijye = 0
                End If
            End If
            l = 0
            'iとjの垂直二等分線の始点
            kx(l) = ijxs  'ks(0)-始点x
            ky(l) = ijys  'ky(0)-始点y
            '計算簡略化のため、領域で交わるかの判断[1]
            saijx = ax(j) - ax(i)
            saijy = ay(j) - ay(i)
            break = 0
            'iとkにたいして同様の作業を繰り返し
            For k = 0 To NNN - 1
                If k <> i And k <> j Then
                    di2 = (ay(i) - ay(k)) / (ax(i) - ax(k))
                    dik = -1 / di2  '二等分線の傾き
                    ciky = (ay(i) + ay(k)) / 2  'y中点
                    cikx = (ax(i) + ax(k)) / 2  'x中点
                    sik = ciky - cikx * dik  '二等分線の切片
                    tik = Dist(ax(i), ay(i), ax(k), ay(k))  '2点間距離
                    ikxs = dik * ijxs + sik   '[ijxs]i,jの二等分線の始点x
                    ikxe = dik * ijxe + sik   '[ijxs]i,jの二等分線の終点x
                    '計算簡略化のため、領域で交わるかの判断[1]
                    sibunten1 = ijys - ikxs
                    sibunten2 = ijye - ikxe
                    saikx = ax(k) - ax(i)  'i,kの中点x
                    saiky = ay(k) - ay(i)  'i,と(i-kの中点y
                    If saijx * saikx > 0 And saijy * saiky > 0 Then 'iからみてkとjが同じ領域にあるならば
                        break3 = 1
                    Else 'iからみてkとjが違うならば
                        break3 = 0
                    End If
                    If sibunten1 * sibunten2 > 0 And tij > tik And break3 = 1 Then  '[i,j]2と[i,k]2が交わる可能性がなければ
                        break = 1
                        Exit For
                    End If
                    If sibunten1 * sibunten2 < 0 Or tij < tik Or break3 <> 0 Then  '[i,j]2と[i,k]2が交わる可能性があれば
                        If sibunten1 * sibunten2 < 0 Or tij > tik Then
                            l = l + 1
                            kx(l) = (sik - sij) / (dij - dik)  '[i,j] [i,k]の交点x
                            ky(l) = dij * kx(l) + sij          '[i,j] [i,k]の交点y
                        End If
                    End If
                End If
            Next k
            
            '[i,j]が他の2等分線と交わっているならば
            If break = 0 Then
                l = l + 1
                '交点に、i,jの画面端点を追加して開始
                kx(l) = ijxe
                ky(l) = ijye
                '計算簡略化のため交点の並び替え
                Call hSort(l, kx, ky)
                '
                For k = 0 To l - 1
                    k2 = k + 1
                    '交点[k]と[k+1]の中点x
                    xx = (kx(k) + kx(k2)) / 2
                    '交点[k]と[k+1]の中点x
                    yy2 = dij * xx + sij
                    '交点[k],[k+1]の中点と点[i]の距離
                    ds = Dist(xx, yy2, ax(i), ay(i))
                    break2 = 0
                    For u = 0 To NNN - 1
                        If u <> i And u <> j Then
                            '交点[k],[k+1]の中点と点[u]の距離
                            us = Dist(xx, yy2, ax(u), ay(u))
                            If us < ds Then
                                break2 = 1
                                Exit For
                            End If
                        End If
                    Next u
                    If break2 = 0 Then
                    'ボロノイ図の描画
                        list(i, j) = Dist(ax(i), ay(i), ax(j), ay(j))
                        Exit For 'break
                    End If
                Next k
            End If
        Next j
        GisMessage ("Line?" + Str(i) + "/" + Str(NNN))
    Next i


    For i = 0 To NNN - 1
        ax(i) = ax(i) + MinX
        ay(i) = ay(i) + MinY
    Next i
    
'   ドローネ三角形の描画
    For i = 0 To NNN - 2
        For j = i + 1 To NNN - 1
        If list(i, j) <> 10 ^ 8 Then
        GisMoveTo (ax(i)), (ay(i)), 0
        GisLineTo (ax(j)), (ay(j)), 0
        GisStoreAsLine
        End If
        Next j
    Next i

'    For i = 0 To NNN - 1
'        For j = 0 To i
'        list(i, j) = list(j, i)
'        Next j
'    Next i
''
'    Open "List.csv" For Output As #5
'
'    For i = 0 To NNN - 1
'        For j = 0 To NNN - 1
'        Write #5, list(i, j);
'        Next j
'        Write #5,
'    Next i
'
'    Call Dikes(list(), NNN)

    MsgBox "End"
End Sub
Public Sub hSort(NN As Integer, crsx() As Variant, crsy() As Variant)
    Dim si As Integer
    Dim kk, kk2, i2, j2, l2 As Integer
    Dim b1, b2, c1, c2 As Variant
    'NN　i,jの2等分線とi,その他2等分線との交点数
    '交点を、xの小さい順から並び替え
    For i2 = 0 To NN - 2
        b1 = crsx(i2)
        b2 = i2
        For j2 = i2 + 1 To NN - 1
            If b1 > crsx(j2) Then
                b1 = crsx(j2)
                b2 = j2
            End If
        Next j2
        If i2 <> b2 Then
            b1 = crsx(i2)
            crsx(i2) = crsx(b2)
            crsx(b2) = b1
            b1 = crsy(i2)
            crsy(i2) = crsy(b2)
            crsy(b2) = b1
        End If
    Next i2
End Sub
Public Sub Dikes(list() As Double, NNN As Integer)

    For k = 0 To NNN - 1
        l = k
        For i = 0 To NNN - 1
            Dike(i, 0) = 10 ^ 8
            Dike(i, 1) = i
            Dike(i, 2) = 0
        Next i
        Dike(l, 2) = 1      '出発点 ID MMM-2 (=NNN)に仮ラベルをつけてスタート
        Dike(l, 0) = 0
        e = 1
        Do Until e >= NNN - 1     'eがMMMになったら終了
            temp6 = 10 ^ 8
            For i = 0 To NNN - 1
                If Dike(i, 2) = 1 And Dike(i, 0) <= temp6 Then  '仮ラベルがついているもののなかで、Pラベルが最小のものを選択[temp6]
                temp = i
                temp6 = Dike(i, 0)
                End If
            Next i
            Dike(temp, 2) = 2               '仮ラベルがついているtemp(ID)を永久ラベルにする
            e = e + 1
            For j = 0 To NNN - 1
                If temp <> j And Dike(j, 0) > Dike(temp, 0) + list(temp, j) Then
                    Dike(j, 0) = Dike(temp, 0) + list(temp, j)
                    Dike(j, 1) = temp
                        If Dike(j, 2) = 0 Then
                            Dike(j, 2) = 1
                        End If
                End If
            Next j
        Loop
        For i = 0 To NNN - 1
            DList(l, i) = Dike(i, 0)
        Next i

    Next k
End Sub

Function kyori(matiID1 As Integer, matiID2 As Integer) As Double
    kyori = DList(matiID1, matiID2)
End Function
Private Function Dist(d1 As Variant, d2 As Variant, d3 As Variant, d4 As Variant) As Variant
    Dist = ((d3 - d1) ^ 2 + (d4 - d2) ^ 2) ^ 0.5
End Function
Private Sub Command2_Click()
    GisAddCommand "program(&Q)|=Doronay", "", "Item", 0, -1, "", ""
    GisRelease
End Sub

Private Sub Form_Load()
    If GisSetupLink(hWnd) = False Then
        MsgBox "Linkに失敗"
        Exit Sub
    End If
End Sub

