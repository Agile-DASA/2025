VERSION 5.00
Begin VB.Form frmDela 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  '�Œ�(����)
   Caption         =   "frmDela1"
   ClientHeight    =   3435
   ClientLeft      =   45
   ClientTop       =   360
   ClientWidth     =   2400
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   229
   ScaleMode       =   3  '�߸��
   ScaleWidth      =   160
   StartUpPosition =   3  'Windows �̊���l
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
    '-------------------------------------�h���[�l�O�p�`�쐬�v���O����-------------------------------------------------
    Const nMax = 500
    Const mMax = 500
    Const lMax = 3000
    Dim NNN, NP As Integer
    Dim MMM As Integer
    Dim id(nMax) As Integer
    'ax,ay�����_���|�C���g�@ad(�|�C���gID)
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
    '�m�[�h��錾
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
    
    '�s�撬�����C���[�̓ǂݍ���
    NNN = GisScanOverlay("DA", 0, "", "")       '���C���[���u0�v
    '�|�C���g��
    temp6 = 10 ^ 8
    temp7 = 10 ^ 8

    For i = 0 To NNN - 1
        id(i) = GisGetListItemInt("DA", Str(i), "_id&")
        ax(i) = GisGetListItemFlt("DA", Str(i), "_ox#")
        ay(i) = GisGetListItemFlt("DA", Str(i), "_oy#")
        ax2(i) = GisGetListItemFlt("DA", Str(i), "_oLat#")
        ay2(i) = GisGetListItemFlt("DA", Str(i), "_oLon#")
        GisMessage ("DP �ǂݍ��ݒ�" + Str(i) + "/" + Str(NNN))
        If (temp6 > ax(i)) Then
        temp6 = ax(i)
        End If
        If (temp7 > ay(i)) Then
        temp7 = ay(i)
        End If
    Next i
    '
    MinX = temp6    'x�̍ŏ��l
    MinY = temp7    'y�̍ŏ��l
    '
    '���------
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
    MaxX = temp8    'x�̍ő�l
    MaxY = temp9    'y�̍ő�l
    
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
    
    'point[all NNN+2��]
    '�|�C���gi�ɑ΂��āA�h���[�l���쐬
    For i = 0 To NNN - 2
        'i�ɑ΂��āAj���Ђ��邩
        For j = i + 1 To NNN - 1
            'i,j�̌X��[di]
            di = (ay(i) - ay(j)) / (ax(i) - ax(j))
            'i,j�̐����񓙕����̌X��[dij]
            dij = -1 / di
            'i,j�̒��_
            cijy = (ay(i) + ay(j)) / 2
            cijx = (ax(i) + ax(j)) / 2
            'i,j�̐ؕ�
            sij = cijy - cijx * dij
            'i,j����
            tij = Dist(ax(i), ay(i), ax(j), ay(j))
            '//[ijxs],[ijys]�����񓙕����̎n�_
            If sij > 0 And sij < MaxY Then '�ؕЂ���ʓ��Ȃ��
                ijxs = 0
                ijys = sij
            Else '��ʊO�Ȃ��
                If dij > 0 Then  '�����񓙕����̌X�����{�Ȃ��
                    ijxs = -sij / dij
                    ijys = 0
                Else        '�����񓙕����̌X�����|�Ȃ��
                    ijxs = (MaxY - sij) / dij
                    ijys = MaxY
                End If
            End If
            '//[ijxe],[ijye]�����񓙕����̏I�_
            yy = dij * MaxX + sij
            If yy > 0 And yy < MaxY Then '�ؕЂ���ʓ��Ȃ��
                ijxe = MaxX
                ijye = yy
            Else  '��ʊO�Ȃ��
                If dij > 0 Then   '�����񓙕����̌X�����{�Ȃ��
                    ijxe = (MaxY - sij) / dij
                    ijye = MaxY
                Else  '�����񓙕����̌X�����|�Ȃ��
                    ijxe = -sij / dij
                    ijye = 0
                End If
            End If
            l = 0
            'i��j�̐����񓙕����̎n�_
            kx(l) = ijxs  'ks(0)-�n�_x
            ky(l) = ijys  'ky(0)-�n�_y
            '�v�Z�ȗ����̂��߁A�̈�Ō���邩�̔��f[1]
            saijx = ax(j) - ax(i)
            saijy = ay(j) - ay(i)
            break = 0
            'i��k�ɂ������ē��l�̍�Ƃ��J��Ԃ�
            For k = 0 To NNN - 1
                If k <> i And k <> j Then
                    di2 = (ay(i) - ay(k)) / (ax(i) - ax(k))
                    dik = -1 / di2  '�񓙕����̌X��
                    ciky = (ay(i) + ay(k)) / 2  'y���_
                    cikx = (ax(i) + ax(k)) / 2  'x���_
                    sik = ciky - cikx * dik  '�񓙕����̐ؕ�
                    tik = Dist(ax(i), ay(i), ax(k), ay(k))  '2�_�ԋ���
                    ikxs = dik * ijxs + sik   '[ijxs]i,j�̓񓙕����̎n�_x
                    ikxe = dik * ijxe + sik   '[ijxs]i,j�̓񓙕����̏I�_x
                    '�v�Z�ȗ����̂��߁A�̈�Ō���邩�̔��f[1]
                    sibunten1 = ijys - ikxs
                    sibunten2 = ijye - ikxe
                    saikx = ax(k) - ax(i)  'i,k�̒��_x
                    saiky = ay(k) - ay(i)  'i,��(i-k�̒��_y
                    If saijx * saikx > 0 And saijy * saiky > 0 Then 'i����݂�k��j�������̈�ɂ���Ȃ��
                        break3 = 1
                    Else 'i����݂�k��j���Ⴄ�Ȃ��
                        break3 = 0
                    End If
                    If sibunten1 * sibunten2 > 0 And tij > tik And break3 = 1 Then  '[i,j]2��[i,k]2�������\�����Ȃ����
                        break = 1
                        Exit For
                    End If
                    If sibunten1 * sibunten2 < 0 Or tij < tik Or break3 <> 0 Then  '[i,j]2��[i,k]2�������\���������
                        If sibunten1 * sibunten2 < 0 Or tij > tik Then
                            l = l + 1
                            kx(l) = (sik - sij) / (dij - dik)  '[i,j] [i,k]�̌�_x
                            ky(l) = dij * kx(l) + sij          '[i,j] [i,k]�̌�_y
                        End If
                    End If
                End If
            Next k
            
            '[i,j]������2�������ƌ�����Ă���Ȃ��
            If break = 0 Then
                l = l + 1
                '��_�ɁAi,j�̉�ʒ[�_��ǉ����ĊJ�n
                kx(l) = ijxe
                ky(l) = ijye
                '�v�Z�ȗ����̂��ߌ�_�̕��ёւ�
                Call hSort(l, kx, ky)
                '
                For k = 0 To l - 1
                    k2 = k + 1
                    '��_[k]��[k+1]�̒��_x
                    xx = (kx(k) + kx(k2)) / 2
                    '��_[k]��[k+1]�̒��_x
                    yy2 = dij * xx + sij
                    '��_[k],[k+1]�̒��_�Ɠ_[i]�̋���
                    ds = Dist(xx, yy2, ax(i), ay(i))
                    break2 = 0
                    For u = 0 To NNN - 1
                        If u <> i And u <> j Then
                            '��_[k],[k+1]�̒��_�Ɠ_[u]�̋���
                            us = Dist(xx, yy2, ax(u), ay(u))
                            If us < ds Then
                                break2 = 1
                                Exit For
                            End If
                        End If
                    Next u
                    If break2 = 0 Then
                    '�{���m�C�}�̕`��
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
    
'   �h���[�l�O�p�`�̕`��
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
    'NN�@i,j��2��������i,���̑�2�������Ƃ̌�_��
    '��_���Ax�̏�������������ёւ�
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
        Dike(l, 2) = 1      '�o���_ ID MMM-2 (=NNN)�ɉ����x�������ăX�^�[�g
        Dike(l, 0) = 0
        e = 1
        Do Until e >= NNN - 1     'e��MMM�ɂȂ�����I��
            temp6 = 10 ^ 8
            For i = 0 To NNN - 1
                If Dike(i, 2) = 1 And Dike(i, 0) <= temp6 Then  '�����x�������Ă�����̂̂Ȃ��ŁAP���x�����ŏ��̂��̂�I��[temp6]
                temp = i
                temp6 = Dike(i, 0)
                End If
            Next i
            Dike(temp, 2) = 2               '�����x�������Ă���temp(ID)���i�v���x���ɂ���
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
        MsgBox "Link�Ɏ��s"
        Exit Sub
    End If
End Sub

