c#	---------------------------�錾-----------------------------------
	Parameter (nMax = 2000)
	Parameter (mMax = 2000)
c#	Parameter (DNum = 297)
c#	Parameter (SNum = 297)
c#	----------------------------ID------------------------------------
c#	[���n�_Id] + [���n�R�X�g]
	Integer DNum,SNum
	Double Precision SId(SNum,2), SId2(SNum,2)
	character*40 Infile
c#	[���v�_��ID]+[���v��]
	Double Precision DId(DNum,2),Okei

c#	�G���A
	Double Precision Area(nMax,nMax),Per(5),GeneG(mMax,nMax)
c#	----------------------�����ݒ�i�ǂݍ��݁j------------------------
c# 	[�z�u�{�ݐ�][���������`��][��`�q�̐�]
	Integer LONum, CNum, GNum,LLONum
c#	----------------------------��`----------------------------------
c#	��`�q�i���n����w�ԍ��j
	Double Precision Gene(mMax,nMax),Gene2(mMax,nMax),GeneP(30)
c#	�]���l
	Double Precision GValue(mMax),GValue2(mMax)
c#	[�ˑR�ψ�][����][�G���[�g�I��]
	Integer VNum, CrNum, ElNum
c#	����,�ψ�,�G���[�g
	Double Precision CGene(nMax, nMax),CGene3(nMax, nMax)
	Double Precision VGene(nMax, nMax),VGene2(nMax, nMax) 
	Double Precision EGene(nMax, nMax) 
c#	---------------------�����E���ڃ}�g���b�N�X-----------------------
c#	���ځ����n�_�̋����}�g���b�N�X
	Double Precision AList(nMax, nMax)
c#	���ۂɍ̗p���ꂽ�����i�Ŋ�{�݂Ƃ̋����j
	Double Precision RList(nMax,2)
	Integer TempH(nMax)
	Double Precision PList(nMax)
c#	--------------------------�X�P�[�����O----------------------------
	Double Precision Gyaku(nMax)
	Double Precision Fmax,Fmin,Favg,ai,bi
	Parameter (w = 2)
c#	�K���x�̕]��
	Double Precision Eval(nMax) 
	Double Precision Para
c#	----------------------------�ꎞ�ϐ�------------------------------
	Double Precision temp,temp1,temp2,temp3,temp4,temp5,cost,cost2
	Integer temp6,temp7,temp8,temp9,b,q,temp10
	Double Precision temp20,temp21,temp22,temp23,temp24
	Integer o,p,e,y,u
	Integer SNumb1,SNumb2
	Double Precision GeneL(20),GeneL2(10,20)
c#	----------------------------�錾�I���---------------------------
c#	|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c#	||||||||||||||||||||||||||���������ǂݍ���|||||||||||||||||||||||
c#	|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c#
c#	
c#	�t�@�C���ǂݍ���
	Write(*,*)'[�����{�݂̗��n�ԍ�������t�@�C��]'
	Read(*,*) Infile

	Write(*,*)	'�����ݒ�̓ǂݍ���'
	Write(*,*) '[���n���̐�][���v�_�̐�]'
	Read(*,*) SNum,DNum
c#
	Write(*,*) '[�z�u�{�ݐ�][���������`��][��`�q�̐�]'
	Read(*,*) LONum,CNum,GNum

c#	�ψق̊����ǂݍ���
	Write(*,*) '[�ˑR�ψ�][����][�G���[�g�I��]'
	Read(*,*) VNum,CrNum,ElNum


c#
c#    �f�[�^�ǂݍ���
c#    [���n�_Id] + [���n�R�X�g]
	open(16,file='SId.txt',status='old',form='formatted')
	Do 74 i=1,SNum
      READ(16,*) (SId(i,j),j=1,2)
74	continue
c# 	[�����}�g���b�N�X]
	open(17,file='DList.txt',status='old',form='formatted')
	DO 30 i=1,DNum
	READ(17,*) (AList(i,j),j=1,SNum)
30	continue
c#
c#	[���v�_��ID] + [���v��]
	open(14,file='DId.txt',status='old',form='formatted')
 	Do 13 i=1,DNum
      READ(14,*) (Did(i,j),j=1,2)
13	continue
c#
	open(15,file=Infile,status='old',form='formatted')
      READ(15,*,End =720) (GeneP(i),i=1,20)
720	continue
	LLONum = i - 1
c#
c#
c#	GA�J�n------------------------------------------------------------
c#	��`�q�̏����z��
	Call Start(mMax,Gene,SId,SId2,SNum,LONum,TempH,nMax,temp,GNum,
     &temp6,SNum,DNum)
c#
c#	���ԍ�2000�@��`���[�v�@�J��Ԃ�
	Do 2000 u = 1,CNum
c#
	Write(*,*) u
c#    �����i�]���l�j�̌v�Z
	Call Dist(mMax,GNum,RList,cost,temp4,LONum,temp6,AList,
     &Gene,DId,DNum,SId,GValue,nMax,SNum,Para,GeneP,LLONum,GeneG,
     &,SNum,DNum)
c#    �]���l�ɂ����ёւ�
	Call rerow(mMax,temp,temp2,GNum,Gene,LONum,nMax,p,q,temp6,temp7,
     &GValue,SNum,DNum)
c#    ���`�X�P�[�����O
	Call scall(mMax,temp,GNum,GValue,LONum,Eval,Gyaku,Favg,Fmax,Fmin,
     &nMax,ai,bi,SNum,DNum)
c#    ��`����
	Call	Inher(mMax,SNum,nMax,Gene,VGene,VGene2,CGene,CGene3,EGene,
     $LONum,
     &CrNum,GNum,ElNum,VNum,Eval,SNumb1,SNumb2,SId,SId2,temp1,temp2,
     &temp3,temp4,temp5,temp6,temp7,temp8,temp9,SNum,DNum)
c#
2000	continue
c#
c#
c#
c#----------------------------------------------------------------------
c#	[cost2]�œK�R�X�g [GeneL]�œK�z�u
	Write(38,*) GValue(1),(Gene(1,i),i=1,LONum)
c#
c#

	Stop
	End
c#
c#
c#	---------------------subroutine+function--------------------------
c#	|||||||||||||||||||||||||�����z��||||||||||||||||||||||||||||||||
	Subroutine Start(mMax,Gene,SId,SId2,SNum,LONum,TempH,nMax,temp,
     &GNum,temp6,SNum,DNum)
	Double Precision Gene(mMax,nMax),temp
	Double Precision SId(nMax,2), SId2(nMax,2)
	integer LONum,GNum,TempH(nMax),temp6
	Integer SNum,DNum
c#
c#	��`�q�̍s��i��j--------------------------
	DO 4099 i=1,GNum
		DO 5099 j=1,LONum
    			Gene(i,j)=0
5099 		continue
4099 	continue
c#    NumGene�̏����z����쐬����----------------
	DO 70 o=1,GNum
c#	�w��ID���R�s�[
		DO 80 p=1,SNum
			SId2(p,1)=SId(p,1)
80		continue
c#	�����z��̍쐬------------------------------------
		DO 90 i=1,LONum
c#		temp6���w�̔z��ԍ� 1-556
			temp6=int(rnd(L)*(SNum-i+1))+1
			Gene(o,i)=SId2(temp6,1)
			DO 100 j=1,SNum-i
				If (j .LT.temp6) then
					TempH(j)=SId2(j,1)
				else
					TempH(j)=SId2(j+1,1)
				end If
100			continue
			DO 110 j=1,SNum-i
				SId2(j,1)=TempH(j)
110			continue
90		continue
70	continue
c#
	Return
	End
c#    ||||||||||||||||||||||||||����||||||||||||||||||||||||||||||||||||
	Subroutine Dist(mMax,GNum,RList,cost,temp4,LONum,temp6,AList,
     &Gene,DId,DNum,SId,GValue,nMax,SNum,Para,GeneP,LLONum,GeneG,
     &SNum,DNum)
c#
	Integer GNum,temp6,LONum,LLONum
	Double Precision RList(nMax,2),AList(nMax,nMax),DId(DNum,2)
	Double precision temp4,cost,SId(SNum,2),GValue(mMax)
	Double Precision Gene(mMax,nMax),Para,GeneG(mMax,nMax)
	Double Precision GeneP(30)
	Integer SNum,DNum
c#
c#	120����̈�`�q'
c#
	Do 900 i = 1,GNum
		Do 901 j = 1,LLONum
			GeneG(i,j) = GeneP(j)
901		continue
		Do 902 j=1,LONum
			GeneG(i,j+LLONum) = Gene(i,j)
902		continue
900	continue
c#'
    	DO 120 i = 1,GNum
c#	�Ŋ��̎��Ə��Ƃ̋���  [���ׂċ�����0]
            DO 130 j = 1,DNum
                RList(j,1) = 0
                RList(j,2) = 0
130		continue
		cost = 0
		temp4= 0
      	Do 140 j = 1,LLONum + LONum
c#		���n�wID��temp�ɕۑ�   [�z��j�Ԗ�]�̗��nID=j=temp
      	      temp6 = GeneG(i, j)           
c#		����(k)�ƍŊ��w(ID:temp2)�̋�����RoomList�Ƀ��X�g��
c#		[���߂Ɍ��肳��闧�n�̂݁A���̂܂܋�����RoomList�ɒǉ�]
      	      If (j.EQ.1) Then
				DO 150 k=1,DNum
      	              RList(k,1) = AList(k, temp6) 
				  RList(k,2) = RList(k,1) * DId(k,2)
150				continue
c#		��ڂɗ��n����闧�n����A�Ŋ�̕����Ƃ̋����ɒu��������
     	      	Else
				DO 160 k=1,DNum
	            	      If (RList(k,1) .GT. AList(k, temp6)) Then
	            	      	RList(k,1) = AList(k, temp6)
				  		RList(k,2) = RList(k,1) * DId(k,2)
	            	      End If
160				continue
            	End If
140		continue
c#
		DO 170 j=1,DNum
			If(temp4.LT.RList(j,1))then
      		temp4 =  RList(j,1)
			End if
170		continue
      	GValue(i) = temp4
120	continue

	Return
	End
c#    |||||||||||||||||||||||||||��`|||||||||||||||||||||||||||||||||||
	Subroutine Inher(mMax,SNum,nMax,Gene,VGene,VGene2,CGene,CGene3,
     &EGene,
     &LONum,CrNum,GNum,ElNum,VNum,Eval,SNumb1,SNumb2,SId,SId2,temp1,
     &temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,SNum,DNum)
c#
	Double Precision SId(nMax,4), SId2(nMax,4)
	Integer LONum,CNum,GNum,ElNum,VNum,temp6,temp7,temp8,temp9
	Double Precision Gene(mMax,nMax)
	Integer  CrNum,SNumb1,SNumb2
	Double Precision CGene(nMax, nMax),CGene3(nMax, nMax)
	Double Precision VGene(nMax, nMax),VGene2(nMax, nMax) 
	Double Precision EGene(nMax, nMax),temp1,temp2,temp3,temp4,temp5
	Double Precision Eval(nMax)
	Integer SNum,DNum
c#    |||||||||||||||||||||||||||||����|||||||||||||||||||||||||||||||||
	Call Cross(mMax,Gene,CrNum,Eval,GNum,LONum,CGene3,nMax,CGene,
     &SNumb1,SNumb2,temp1,temp2,temp3,temp4,temp5,temp6,temp7,SNum,
     &temp9,SNum,DNum)
c#    ||||||||||||||||||||||||||||�ˑR�ψ�||||||||||||||||||||||||||||||
	Call	Vate(mMax,VNum,Gene,nMax,temp3,Eval,GNum,VGene,LONum,temp1,
     &temp2,SNum,temp4,SId,VGene2,temp6,temp7,temp8,temp9,SNum,DNum)
c#    ||||||||||||||||||||||||�G���[�g�I��||||||||||||||||||||||||||||||
	Call Elie(mMax,nMax,EGene,Gene,ElNum,LONum,SNum,DNum)

c#    |||||||||||||||||||||||||||����|||||||||||||||||||||||||||||||||||
	Call Sele(mMax,nMax,LONum,ElNum,CrNum,GNum,Gene,EGene,CGene3,
     &VGene2,SNum,DNum)
	Return
	End
c#    |||||||||||||||||||||||||||||����|||||||||||||||||||||||||||||||||
	Subroutine Cross(mMax,Gene,CrNum,Eval,GNum,LONum,CGene3,nMax,
     &CGene,SNumb1,SNumb2,temp1,temp2,temp3,temp4,temp5,temp6,temp7,
     &SNum,temp9,SNum,DNum,SNum,DNum)
c#
c#	����
	Double Precision CGene(nMax, nMax)
	Double Precision CGene3(nMax, nMax),Gene(mMax,nMax)
	Integer CrNum,GNum,LONum,temp6,temp7
	Double Precision Eval(nMax)
	Integer SNumb1,SNumb2,temp9
	Double Precision temp1,temp2,temp5,temp3,temp4
	Integer SNum,DNum

	 Do 440 v=1,CrNum+10,2
c#	���ԍ�601�������I��
		If(v.GT.CrNum)then goto 601
		End If
302	continue
c#    ����������y�A��I��
c#    ���[���b�g
	            temp1 = rnd(L) * Eval(GNum)
	            temp2 = rnd(L) * Eval(GNum)
c#    �I�����ꂽ��`�q���݂���
c#	temp3,temp4	���������`�q[1-GNum]
		If((0.LE.temp1).And.(temp1.LT.Eval(1))) Then
			temp6=1
			goto 451
		Else 
		Do 450 i=1,GNum-1
      	If ((Eval(i) .LE. temp1).And.(temp1.LT.Eval(i + 1)))Then
      	    	temp6 = i+1
                	goto 451
                End If
450   	continue
		End if 

451   continue
	
		If((0.LE.temp2).And.(temp2.LT.Eval(1))) Then
			temp7=1
			goto 441
		Else
		Do 460 j=1,GNum-1
                If ((Eval(j).LE. temp2).And.(temp2.LT.Eval(j + 1))) Then
                  temp7 = j+1
                  goto 441
                End If
460		continue
		End if
441	continue
	If(temp6.EQ.temp7) then goto 302
	End If
c#    ======================���������`�q���R�s�[======================
c#		======================�����̊J�n============================
c#    	�q���̈�`�qChildGene
c#    	�q���̈�`�q�̗��n�ԍ� ChildGene2
	Do 510 i=1,LONum
	            CGene(1, i) = Gene(temp6,i)
			CGene(2, i) = Gene(temp7,i)
			CGene(3, i)=1
			CGene(4, i)=1
			CGene(5, i)=0
510	continue
	Do 1034 j=1,SNum
		CGene(6,j)=0
		CGene(7,j)=j
		CGene(8,j)=0
1034	continue
c#    	=========��`�q1�̂����ň�`�q2�Əd�����闧�n�ԍ���T��===
c#			CGene(temp3,i)
c#			CGene(temp4,i)
c#    	=========��`�q1�̂����ň�`�q2�Əd���̂Ȃ����n�ԍ���T��===
c#                �u1�v�ˏd���Ȃ��@�u0�v�ˏd������
		Do 820 i=1,LONum 
	      	SNumb1 = CGene(1, i)
c#			�����������n�ԍ�������΁@�u0�v
			Do 830 j=1,LONum
	            	If (CGene(2, j).EQ.SNumb1) Then
	                  	CGene(3, i) = 0
					CGene(4, j) = 0
	                        goto 831
	                  End If
830			continue
831			continue
820		continue
c#	------------------------------------------------------------------
	temp9=0
	temp6=0
	temp7=SNum
c#	����`�q1�̏�����
c#	A.�������n��CGene(5,temp9)
c#	temp5 �������n�ԍ��̐��A�܂�CGene(5,temp9)�̔z��̗v�f��
			Do 530 i=1,LONum
				SNumb1=CGene(1,i)
	            	If (CGene(3,i).EQ.0) Then
					temp9=temp9+1
	                  	CGene(5,temp9) = SNumb1
c#	B.���������`�q�Ɋ܂܂�Ȃ����n��CGene(7,temp7) �������@
c#	temp7�@�܂܂�Ȃ����n�̐��A�܂�CGene(7,temp7)�̔z��̗v�f��
					temp7=temp7-1
					Do 533 k=1,temp7
						If(CGene(7,k).LT.SNumb1)then
							CGene(8,k)=CGene(7,k)
						Else
							CGene(8,k)=CGene(7,k+1)
						End if
533					continue
					Do 536 k=1,temp7
						CGene(7,k)=CGene(8,k)
536					continue
c#	C.�قȂ闧�n��CGene(6,temp6)
c#	temp6 �قȂ闧�n�̐��A�܂�CGene(6,temp6)�̔z��̗v�f��
				Else
					temp6=temp6+1
					CGene(6,temp6) = SNumb1

c#	B.���������`�q�Ɋ܂܂�Ȃ����n��CGene(7,temp7)�@�������@
c#	temp7�@�܂܂�Ȃ����n�̐��A�܂�CGene(7,temp7)�̔z��̗v�f��
					temp7=temp7-1
					Do 544 k=1,temp7
						If(CGene(7,k).LT.SNumb1)then
							CGene(8,k)=CGene(7,k)
						Else
							CGene(8,k)=CGene(7,k+1)
						End if
544					continue
					Do 537 k=1,temp7
						CGene(7,k)=CGene(8,k)
537					continue

	                  End If
530			continue
c#	����`�q2�̏�����
c#			�قȂ闧�n�ԍ��́�CGene(6,temp6)
			Do 550 j=1,LONum
c#	C.�قȂ闧�n��CGene(6,temp6)
c#	temp6 �قȂ闧�n�̐��A�܂�CGene(6,temp6)�̔z��̗v�f��
            		If (CGene(4,i).EQ.1) Then
	      			SNumb2 = CGene(2, i)
				      temp6 = temp6+1
             		     	CGene(6,temp6) = SNumb2
c#	B.���������`�q�Ɋ܂܂�Ȃ����n��CGene(7,temp7)�@�������@
c#	temp7�@�܂܂�Ȃ����n�̐��A�܂�CGene(7,temp7)�̔z��̗v�f��
					temp7 = temp7-1
					Do 542 k=1,temp7
						If(CGene(7,k).LT.SNumb2)then
							CGene(8,k)=CGene(7,k)
						Else
							CGene(8,k)=CGene(7,k+1)
						End if
542					continue
					Do 538 k=1,temp7
						CGene(7,k)=CGene(8,k)
538					continue
             	    	End If
550			continue
c#	�d�����Ă��闧�n�̐���temp9  CGene(5,temp9)
c#	�قȂ闧�n�̐�      ��temp6  CGene(6,temp6)
c#	���̏W�c�̗��n�̐�  ��temp7 CGene(7,SNum-temp7)
c#	<����>
c#	A.�d�����Ă��闧�n�͑S��(temp9��)������`
	Do 560 i=1,temp9
		temp1 =CGene(5,i) 
		CGene3(v,i)=temp1
		CGene3(v+1,i)=temp1
560	continue
	If(temp9.EQ.LONum) then
	goto 569
	End if
c#	�R�s�[
	Do 583 i=1,temp6
		CGene(10,i)=CGene(6,i)
583	continue
	Do 584 i=1,temp7
		CGene(11,i)=CGene(7,i)
584	continue
c#	<��`�q1>
c#	B.�قȂ闧�n��<temp2��>�݈̂�` temp6
c#	��`�����闧�n��  CGene(6,temp1)
		temp2=int((LONum-temp9)*2/3)
		temp3=0
		Do 570 i=temp9+1,temp9+temp2
			temp1=int(rnd(L)*(temp6-temp3))+1
			CGene3(v,i)=CGene(6,temp1)
			temp3=temp3+1
			Do 579 j=temp1,temp6-temp3
				CGene(6,j)=CGene(6,j+1)
579			continue
570		continue
c#	C.���̏W�c����͎c��<LONum-temp9-temp2>��` temp7
c#	��`�����闧�n�́@CGene(7,temp1)
		temp3=0
		Do 580 i=temp9+temp2+1,LONum
			temp1=int(rnd(L)*(temp7-temp3))+1
			CGene3(v,i)=CGene(7,temp1)
			temp3=temp3+1
			Do 515 j=temp1,temp7-temp3
				CGene(7,j)=CGene(7,j+1)
515			continue
580		continue
c#	<��`�q2>
c#	B.�قȂ闧�n��<temp2��>�݈̂�` temp6
c#	��`�����闧�n��  CGene(6,temp1)
		temp2=int((LONum-temp9)*2/3)
		temp3=0
		Do 511 i=temp9+1,temp9+temp2
			temp1=int(rnd(L)*(temp6-temp3))+1
			CGene3(v+1,i)=CGene(10,temp1)
			temp3=temp3+1
			Do 512 j=temp1,temp6-temp3
				CGene(10,j)=CGene(10,j+1)
512			continue
511		continue
c#	C.���̏W�c����͎c��<LONum-temp9-temp2>��` temp7
c#	��`�����闧�n�́@CGene(7,temp1)
		temp3=0
		Do 513 i=temp9+temp2+1,LONum
			temp1=int(rnd(L)*(temp7-temp3))+1
			CGene3(v+1,i)=CGene(11,temp1)
			temp3=temp3+1
			Do 514 j=temp1,temp7-temp3
				CGene(11,j)=CGene(11,j+1)
514			continue
513		continue
569	continue
c#	440�@�����@�J��Ԃ�
440	continue
c#	601�@�����@�I��
601	continue
	Return
	End

c#    ||||||||||||||||||||||||||||�ˑR�ψ�||||||||||||||||||||||||||||||
	Subroutine Vate(mMax,VNum,Gene,nMax,temp3,Eval,GNum,VGene,LONum,
     &temp1,temp2,SNum,temp4,SId,VGene2,temp6,temp7,temp8,temp9,
     &SNum,DNum)

	Integer VNum,LONum,GNum,temp6,temp7,temp8,temp9
	Double Precision Gene(mMax,nMax),VGene(nMax,nMax),temp1,temp2
	Double Precision Eval(nMax),SId(nMax,4),temp4,temp3
	Double Precision VGene2(nMax,nMax)
	Integer SNum,DNum

	Do 610 v=1,VNum+5

c#	���ԍ�701�˓ˑR�ψُI��
		If(v.GT.VNum) then goto 701
		End If
c#		'�ψق����`�q������ 1-GNum
c#		temp3
	      temp3 = rnd(L) * Eval(GNum)
		If((0.LE.temp3).And.(temp3.LT.Eval(1))) Then
			temp6=1
			goto 640
		Else

		Do 630 i=1,GNum-1
                If ((Eval(i).LE.temp3).And. (temp3.LT.Eval(i + 1))) Then
                    temp6 = i
                    goto 640
                End If
630		continue
		End if
640         continue
c#
		If (temp6.GT.GNum)Then
                temp6 = Int(rnd(L) * GNum)+1
            End If            
		Do 650 j=1,LONum
                VGene(temp6, j) = Gene(temp6, j)
650		continue
c#
c#		���������`�q�̏ꏊ[�z��ԍ�]���w�� 1-556
c#		ex  a1,a2,a3,[a4,a5,a6,a7],a8,a9
c#		(temp1 - temp2)
620		continue
                	temp7 = Int(rnd(L) * LONum)+1
                	temp8 = Int(rnd(L) * LONum)+1
		If(temp7.LT.temp8) then goto 620
		end If
c#    	===========�ψيJ�n=====================================
			Do 660 j=temp7,temp8
c#		�ψقŒ�������w�̔z�������      temp4 1-556
690         	continue   
			temp9 = Int(rnd(L) * SNum)+1
				Do 670 i=1,LONum
c#				���肵���w�ԍ����d�����Ă��邩�̌���
c#		   		*�����d�����Ă���Ȃ�� �A�w�̍đI��
                    	If (VGene(temp6, i).EQ.SId(temp9,1)) Then
                        	goto 690
                    	End If
670				continue
c#				*�d�����Ȃ����
                  	VGene(temp6, j) = SId(temp9,1)
660			continue		
		Do 700 i=1,LONum
                VGene2(v, i) = VGene(temp6, i)
700		continue
c#	610�@�ˑR�ψف@�J��Ԃ�
610	continue
c#	701�@�ˑR�ψف@�I��
701	continue
	Return
	End
c#    ||||||||||||||||||||||||�G���[�g�I��||||||||||||||||||||||||||||||
	Subroutine Elie(mMax,nMax,EGene,Gene,ElNum,LONum,SNum,DNum)
	Integer ElNum,LONum
	Double Precision EGene(nMax,nMax),Gene(mMax,nMax)
	Integer SNum,DNum
c#
	Do 710 i=1,ElNum
		Do 720 j=1,LONum
            	EGene(i, j) = Gene(i, j)
720	continue
710	continue
	Return
	End
c#    |||||||||||||||||||||||||||����|||||||||||||||||||||||||||||||||||
	Subroutine Sele(mMax,nMax,LONum,ElNum,CrNum,GNum,Gene,EGene,CGene3,
     &VGene2,SNum,DNum)
	Integer LONum,ElNum,CrNum,GNum
	Double Precision Gene(mMax,nMax),EGene(nMax,nMax)
	Double Precision CGene3(nMax,nMax),VGene2(nMax,nMax)
	Integer SNum,DNum
c#
      Do 730 j=1,LONum
		Do 740 i=1,ElNum
            	Gene(i, j) = EGene(i, j)
740		continue       
		Do 750 i=ElNum+1,ElNum + CrNum
            	Gene(i, j) = CGene3(i - ElNum, j)
750		continue        
		Do 760 i=ElNum + CrNum + 1,GNum
            	Gene(i, j) = VGene2(i - ElNum - CrNum, j)
760		continue
730	continue
	Return
	End

c#    ==================�����������������ɕ��ёւ�======================
	Subroutine rerow(mMax,temp,temp2,GNum,Gene,LONum,nMax,p,q,temp6,
     &temp7,GValue,SNum,DNum)
c#
	Double Precision Gene(mMax,nMax),GValue(mMax)
	Double Precision temp,temp2
	Integer LONum,GNum,p,temp6,temp7,q
	Integer SNum,DNum
c#
	DO 180 i=1,GNum-1
      	temp = GValue(i)
		temp6 = i
        	DO 190 j=i+1,GNum
            If (GValue(j) .LT. temp) Then
            	temp = GValue(j)
                	temp6 = j
            End If
190		continue
c#
		If(i.NE.temp)then
			DO 200 k=1,LONum
	                  temp7 = Gene(i, k)
	                  Gene(i, k) = Gene(temp6, k)
	                  Gene(temp6, k) = temp7
200			continue
			temp2 = GValue(i)
			GValue(i) = GValue(temp6)
			GValue(temp6) = temp2
		End if
180	continue

	Return
	End
c#    |||||||||||||||||||||||||�X�P�[�����O||||||||||||||||||||||||||||
	Subroutine scall(mMax,temp,GNum,GValue,LONum,Eval,Gyaku,Favg,
     &Fmax,Fmin,nMax,ai,bi,SNum,DNum)
	Double Precision GValue(mMax),Gyaku(nMax),Eval(nMax)
	Double Precision temp,Favg,Fmax,Fmin
	Integer GNum,LONum
	Double Precision ai,bi
	Integer SNum,DNum
	
	temp=0
	DO 3009 i=1,GNum
      	Gyaku(i) = 1 / GValue(i)
        	temp = temp + Gyaku(i)
3009	continue
    	Favg = 0
    	Fmax = 0
    	Fmin = 0
c#
    	Favg = temp / GNum
    	Fmax = Gyaku(1)
    	Fmin = Gyaku(GNum)
	If (Fmax .GT. (w * Favg - Fmax) / (w - 1)) Then
      	ai = ((w - 1) * Favg) / (Fmax - Favg)
            bi = (Favg * (Fmax - w * Favg)) / (Fmax - Favg)
        	Else
        	ai = Favg / (Favg - Fmin)
        	bi = Favg * Fmin / (Favg - Fmin)
    	End If
c#
	DO 4009 i=1,GNum
      	Gyaku(i) = ai * Gyaku(i) + bi
4009	continue
c#    �K���x�̕]��
    	Eval(1) = Gyaku(1)
	DO 4109 i=2,GNum
       	 Eval(i) = Eval(i - 1) + Gyaku(i)
4109	continue
	Return
	End
c#	-------------------------Function�֐�-----------------------------
	FUNCTION Rnd(L)
	INTEGER A,P,X
	SAVE X
	DATA X/759375/
	DATA A/15/
	DATA P/1000001/
	X=MOD(A*X,P)
	Rnd=X/FLOAT(P)
c#	ram=int(Rnd*1000)
	RETURN
	END    
