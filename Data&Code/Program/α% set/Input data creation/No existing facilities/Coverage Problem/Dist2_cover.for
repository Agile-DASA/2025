c#	---------------------------宣言-----------------------------------
	Parameter (nMax = 2000)
	Parameter (mMax = 2000)
	Parameter (DNum = 297)
	Parameter (SNum = 297)
c#	----------------------------ID------------------------------------
c#	[立地点Id] + [立地コスト]
	Double Precision SId(SNum,2)
	Double Precision DId(DNum,2)
c#	バッファ単位
	Double Precision Area(nMax,nMax)
	Integer GeneG(nMax)
c#	Double Precision Area2(SNum,SNum)
c#	----------------------初期設定（読み込み）------------------------
c# 	[配置施設数][収束する遺伝回数][遺伝子の数]
	Integer LONum,LLONum
c#	----------------------------遺伝----------------------------------
c#	遺伝子（立地する駅番号）
	Integer Gene(nMax),GeneP(30)
c#	評価値
	Double Precision GValue
c#	[突然変異][交叉][エリート選択]
	Integer VNum, CrNum, ElNum
c#	交叉,変異,エリート
c#	---------------------距離・丁目マトリックス-----------------------
c#	丁目＊立地点の距離マトリックス
	Double Precision AList(nMax, nMax)
c#	実際に採用された距離（最寄施設との距離）
	Double Precision RList(nMax,2)
	Integer TempH(nMax)
	Double Precision PList(nMax)
c#	----------------------------一時変数------------------------------
	Double Precision temp,temp1,temp2,temp3,temp4,temp5,cost,cost2
	Integer temp6,temp7,temp8,temp9,b,q,temp10
	Double Precision temp20,temp21,temp22,temp23,temp24,DistMin
	Double Precision Minimum(3),cover
	Integer FileNum
	Integer o,p,e,y,u
	Integer SNumb1,SNumb2
	Integer aint,bint,cint
c#	----------------------------宣言終わり---------------------------
c#	|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c#	||||||||||||||||||||||||||初期条件読み込み|||||||||||||||||||||||
c#	|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c#

	Write(*,*)'カバー範囲'
	Read(*,*) cover

c#    データ読み込み
c#    [立地点Id] + [立地コスト]
	Write(*,*)'最小値'
	Read(*,*) DistMin
c#
	open(16,file='SId.txt',status='old',form='formatted')
	Do 74 i=1,SNum
      READ(16,*) (SId(i,j),j=1,2)
74	continue
c# 	[距離マトリックス]
	open(17,file='DList.txt',status='old',form='formatted')
	DO 30 i=1,DNum
	READ(17,*) (AList(i,j),j=1,SNum)
30	continue
c#
c#	[需要点のID] + [需要量]
	open(14,file='DId.txt',status='old',form='formatted')
 	Do 13 i=1,DNum
      READ(14,*) (Did(i,j),j=1,2)
13	continue
c#

	LONum=2
c#
c#	------------------------------------------------------------
c#
	FileNum = 80
	Write(FileNum,*) 'cost='
	Write(FileNum+1,*) 'cost='
	Write(FileNum+2,*) 'cost='
	Write(FileNum+3,*) 'cost='

	Do 601 aint=1,SNum
		Do 602 bint=1,SNum
		Gene(1) = aint
		Gene(2) = bint
c#

c#	最寄りの事業所との距離  [すべて距離を0]
            DO 130 j = 1,DNum
                RList(j,1) = 0
                RList(j,2) = 0
130		continue
		GValue= 0
      	Do 140 j = 1,LONum
c#		立地駅IDをtempに保存   [配列j番目]の立地ID=j=temp
      	      temp6 = Gene(j)  
c#		丁目(k)と最寄り駅(ID:temp2)の距離をRoomListにリスト化
c#		[初めに決定される立地のみ、そのまま距離をRoomListに追加]
      	      If (j.EQ.1) Then
				DO 150 k=1,DNum
      	              RList(k,1) = AList(k, temp6) 
150				continue
c#		二つ目に立地される立地から、最寄の物件との距離に置きかえる
     	      	Else
				DO 160 k=1,DNum
	            	      If (RList(k,1) .GT. AList(k, temp6)) Then
	            	      	RList(k,1) = AList(k, temp6)
	            	      End If
160				continue
            	End If
140		continue
c#
		DO 170 j=1,DNum
			If(RList(j,1).LT.cover)then
      		GValue =  GValue + DId(j,2)
			End if
170		continue

		If(GValue.GT.DistMin*0.95)then
		Write(FileNum,*) '[',aint,',',bint,']', 0
		Else 
      	Write(FileNum,*) '[',aint,',',bint,']', 1
		End if

		If(GValue.GT.DistMin*0.9)then
		Write(FileNum+1,*) '[',aint,',',bint,']', 0
		Else 
      	Write(FileNum+1,*) '[',aint,',',bint,']', 1
		End if

		If(GValue.GT.DistMin*0.85)then
		Write(FileNum+2,*) '[',aint,',',bint,']', 0
		Else 
      	Write(FileNum+2,*) '[',aint,',',bint,']', 1
		End if
		
		If(GValue.GT.DistMin*0.8)then
		Write(FileNum+3,*) '[',aint,',',bint,']', 0
		Else 
      	Write(FileNum+3,*) '[',aint,',',bint,']', 1
		End if

602		continue
c#		bint
	Write(*,*) GValue
601	continue
c#	aint

	Write(FileNum,*) ';'
	Write(FileNum+1,*) ';'
	Write(FileNum+2,*) ';'
	Write(FileNum+3,*) ';'
	Stop
	End



c#	-------------------------Function関数-----------------------------
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
