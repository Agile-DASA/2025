c#	---------------------------�錾-----------------------------------
	Parameter (nMax = 2000)
	Parameter (mMax = 2000)
	Parameter (DNum = 297)
	Parameter (SNum = 297)
c#	----------------------------ID------------------------------------
c#	[���n�_Id] + [���n�R�X�g]
	Double Precision SId(SNum,2)

	Double Precision DId(DNum,2)
c#	�o�b�t�@�P��
	Integer Area(nMax,nMax)
	Integer AreaNum(4)
	Integer GeneG(nMax)
c#	Double Precision Area2(SNum,SNum)
c#	----------------------�����ݒ�i�ǂݍ��݁j------------------------
c# 	[�z�u�{�ݐ�][���������`��][��`�q�̐�]
	Integer LONum,LLONum
c#	----------------------------��`----------------------------------
c#	��`�q�i���n����w�ԍ��j
	Integer Gene(nMax),GeneP(30)
c#	�]���l
	Double Precision GValue
c#	[�ˑR�ψ�][����][�G���[�g�I��]
	Integer VNum, CrNum, ElNum
c#	����,�ψ�,�G���[�g
c#	---------------------�����E���ڃ}�g���b�N�X-----------------------
c#	���ځ����n�_�̋����}�g���b�N�X
	Double Precision AList(nMax, nMax)
c#	���ۂɍ̗p���ꂽ�����i�Ŋ�{�݂Ƃ̋����j
	Double Precision RList(nMax,2)
	Integer TempH(nMax)
	Double Precision PList(nMax)
	Double Precision bafa
c#	----------------------------�ꎞ�ϐ�------------------------------
	Double Precision temp,temp1,temp2,temp3,temp4,temp5,cost,cost2
	Integer temp6,temp7,temp8,temp9,b,q,temp10
	Double Precision temp20,temp21,temp22,temp23,temp24,DistMin
	Integer Itemp1,Itemp2
	Integer FileNum
	Integer o,p,e,y,u
	Integer SNumb1,SNumb2
	Integer aint,bint,cint,dint
	Integer point(4)
c#	----------------------------�錾�I���---------------------------
c#	|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c#	||||||||||||||||||||||||||���������ǂݍ���|||||||||||||||||||||||
c#	|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c#

	Write(*,*)'�ŏ��l'
	Read(*,*)DistMin
c#
	Write(*,*)'�œK�n�_1,�œK�n�_2,�œK�n�_3,�œK�n�_4'
	Read(*,*) point(1),point(2),point(3),point(4)
c#
	Write(*,*)'�o�b�t�@'
	Read(*,*)bafa

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

c#
c#	------------------------------------------------------------
c#
	LONum = 4
	Do 1300 i=1,LONum
	Itemp1 = 0
	Do 1301 j=1,DNum
		If(AList(point(i),j).LE.bafa)then
		Itemp1 = Itemp1 + 1
		Area(i,Itemp1)=j
		End if
1301	continue
	AreaNum(i) = Itemp1
1300	continue

	Write(*,*) (AreaNum(i),i=1,LONum)

	FileNum = 65
	Write(FileNum,*) 'cost='
	Write(FileNum+1,*) 'cost='
	Write(FileNum+2,*) 'cost='
	Write(FileNum+3,*) 'cost='

	Do 601 aint=1,AreaNum(1)
		Do 602 bint=1,AreaNum(2)
		Do 603 cint=1,AreaNum(3)
		Do 604 dint=1,AreaNum(4)
c#
		Gene(1) = Area(1,aint)
		Gene(2) = Area(2,bint)
		Gene(3) = Area(3,cint)
		Gene(4) = Area(4,dint)
c#



c#	�Ŋ��̎��Ə��Ƃ̋���  [���ׂċ�����0]
            DO 130 j = 1,DNum
                RList(j,1) = 0
                RList(j,2) = 0
130		continue
		GValue= 0
      	Do 140 j = 1,LONum
c#		���n�wID��temp�ɕۑ�   [�z��j�Ԗ�]�̗��nID=j=temp
      	      temp6 = Gene(j)  
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
      		GValue = GValue + RList(j,2)
170		continue

		If(GValue.LT.DistMin*1.05)then

		Write(FileNum,*) '[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']', 0
		Else 
      	Write(FileNum,*)'[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']',1

		End if

		If(GValue.LT.DistMin*1.1)then
		Write(FileNum+1,*) '[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']', 0
		Else 
      	Write(FileNum+1,*)'[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']',1
		End if

		If(GValue.LT.DistMin*1.15)then
		Write(FileNum+2,*) '[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']', 0
		Else 
      	Write(FileNum+2,*)'[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']',1
		End if
		
		If(GValue.LT.DistMin*1.2)then
		Write(FileNum+3,*) '[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']', 0
		Else 
      	Write(FileNum+3,*)'[',Area(1,aint),',',Area(2,bint),',',Area(3,cint),',',Area(4,dint),']',1
		End if

604		continue
c#		dint
603		continue
c#		cint
602		continue
c#		bint
	Write(*,*) aint
601	continue
c#	aint

	Write(FileNum,*) ';'
	Write(FileNum+1,*) ';'
	Write(FileNum+2,*) ';'
	Write(FileNum+3,*) ';'
          
	Stop
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
