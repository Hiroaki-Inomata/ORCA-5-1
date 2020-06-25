      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------------*
      *    ϫ�ҥ������ִ����ơ��֥� COPY (CMRSICNGTBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.00    NACL-¿��Ǽ  13/11/01  ϫ���ŻҲ��û��ɲ�
      *****************************************************************
      *
       01  TBL-RSICHG-AREA.
      *    ϫ�Ҥεߵް��Ŵ����û�
           03  FILLER      PIC X(20)   VALUE   "R101110020 101800880".
           03  FILLER      PIC X(20)   VALUE   "R101110030 101800890".
      *    ϫ�Ҥ����ܤε��������谷��
           03  FILLER      PIC X(20)   VALUE   "R101130020 101800870".
      *    ϫ�Ҥν�ǻ��֥�å�����
           03  FILLER      PIC X(20)   VALUE   "R101400030 101400030".
      *    ϫ�Ҥν�ǻ��֥�å������ʼ�ѡ�
           03  FILLER      PIC X(20)   VALUE   "R101400030 101500150".
      *    ϫ�Ҥμ�ؤ��Ͻ��˷��뵡ǽ������Ƴ�û�
           03  FILLER      PIC X(20)   VALUE   "R101500080 101500080".
      *
      *    ϫ�Ҥκƽ�ϫ���ܻ�Ƴ������
           03  FILLER      PIC X(20)   VALUE   "A101130010 101800920".
           03  FILLER      PIC X(20)   VALUE   "A101130005 101800910".
      *
      *    �ƿ���
           03  FILLER      PIC X(20)   VALUE   " 101120040 101120010".
      *    ��������û����ɤ��ؤ���
           03  FILLER      PIC X(20)   VALUE   " 101120070 101120020".
      *    �ƿ�����Ʊ���������ܡ�
           03  FILLER      PIC X(20)   VALUE   " 101120100 101120040".
      *
      *    �ƿǻ����ܻ�Ƴ������
           03  FILLER      PIC X(20)   VALUE   " 101120030 101130190".
      *    �ٶȾ���
           03  FILLER      PIC X(20)   VALUE   " 101130030 101800470".
      *    �Ǹ����
           03  FILLER      PIC X(20)   VALUE   " 101130040 101800480".
      *    ���ǽ���
           03  FILLER      PIC X(20)   VALUE   " 101130050 101800490".
      *    ���ǽ����ʤϤꡦ���夦��
           03  FILLER      PIC X(20)   VALUE   " 101130060 101800500".
      *    ���ǽ����ʰ������ܤȤϤꡦ���夦��
           03  FILLER      PIC X(20)   VALUE   " 101130070 101800510".
      *    ���ǽ�����ɾ��ɽź�ա�
           03  FILLER      PIC X(20)   VALUE   " 101130080 101800520".
      *    ���ǽ����ʥޥå�������
           03  FILLER      PIC X(20)   VALUE   " 101130090 101800530".
      *    ��������˵��տ��ǽ���
           03  FILLER      PIC X(20)   VALUE   " 101130100 101800540".
      *    ��ϥӥ�ơ����������󶡲û�
           03  FILLER      PIC X(20)   VALUE   " 101130110 101130200".
      *    �����ɼ�����ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800080 101800230".
      *    �����ɼ�����ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800090 101800240".
      *    Ǿ��ɼ�������ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800100 101800250".
      *    Ǿ��ɼ�������ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800110 101800260".
      *    Ǿ��ɼ�������ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800120 101800270".
      *    ��ư���ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800125 101800280".
      *    ��ư���ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800130 101800290".
      *    ��ư���ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800140 101800330".
      *    �Ƶ۴��ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800150 101800340".
      *    �Ƶ۴��ϥӥ�ơ���������ʣ���
           03  FILLER      PIC X(20)   VALUE   " 101800160 101800350".
      *    ���ģ̲û�
           03  FILLER      PIC X(20)   VALUE   " 101800170 101800360".
      *    ��������ˬ���Ƴ��
           03  FILLER      PIC X(20)   VALUE   " 101800175 101800930".
      *    ��������ˬ���Ƴ���ʤ���¾�μ����ξ���
           03  FILLER      PIC X(20)   VALUE   " 101800180 101800940".
      *    ��������ˬ���Ƴ���ʰۤʤ뿦��Ͱʾ�˲û�
           03  FILLER      PIC X(20)   VALUE   " 101800190 101800950".
      *    ϫ�ҥ�ϡʣ������ܡ�
           03  FILLER      PIC X(20)   VALUE   " 101800200 101800080".
      *    ����ƥ���
           03  FILLER      PIC X(20)   VALUE   " 101800210 101800090".
      *    �������̻�Ƴ������
           03  FILLER      PIC X(20)   VALUE   " 101800220 101800100".
      *    ���żҲ�������Ƴ������
           03  FILLER      PIC X(20)   VALUE   " 101800230 101800110".
      *    �̼���ϥƥ���
           03  FILLER      PIC X(20)   VALUE   " 101800240 101800120".
      *    �����ư�踡��
           03  FILLER      PIC X(20)   VALUE   " 101800250 101800130".
      *    �����ƥ���
           03  FILLER      PIC X(20)   VALUE   " 101800260 101800140".
      *    �����ƥ���
           03  FILLER      PIC X(20)   VALUE   " 101800270 101800150".
      *    �����ƥ��ȡ����Ƚ������ˤ��ʣ���ʤ�Ρ�
           03  FILLER      PIC X(20)   VALUE   " 101800280 101800120".
      *    ��������ư���
           03  FILLER      PIC X(20)   VALUE   " 101800290 101800170".
      *    ��������
           03  FILLER      PIC X(20)   VALUE   " 101800300 101800180".
      *    ���̽�ϫ����
           03  FILLER      PIC X(20)   VALUE   " 101800310 101800190".
      *    �����󥻥��
           03  FILLER      PIC X(20)   VALUE   " 101800320 101800200".
      *    �������������
           03  FILLER      PIC X(20)   VALUE   " 101800330 101800210".
      *    ������ν�����
           03  FILLER      PIC X(20)   VALUE   " 101800340 101800220".
      *    �����ʿ��������ٱ�û�
           03  FILLER      PIC X(20)   VALUE   " 101800350 101800370".
      *    ���ʼ������ܴ�����
           03  FILLER      PIC X(20)   VALUE   " 101800360 101130160".
      *    ���ʼ���ϫ�������Ƴ��
           03  FILLER      PIC X(20)   VALUE   " 101800370 101800900".
      *    �����ѿ���������
           03  FILLER      PIC X(20)   VALUE   " 788888001 788888003".
      *    ����������
           03  FILLER      PIC X(20)   VALUE   " 788888002 788888004".
      *    ���Ǹ����ѥ�����
           03  FILLER      PIC X(20)   VALUE   " 788888003 788888007".
      *    ����������
           03  FILLER      PIC X(20)   VALUE   " 788888004 788888008".
      *    ɨ��­���������������
           03  FILLER      PIC X(20)   VALUE   " 788888005 788888009".
      *H25.10
      *    ϫ���ŻҲ��û����ݸ����
           03  FILLER      PIC X(20)   VALUE   "A101801000 101801000".
      *
       01  TBL-RSICHG-AREA-R       REDEFINES   TBL-RSICHG-AREA.
           03  TBL-RSICHG-OCC      OCCURS   59  INDEXED   TBL-RCN.
      *        (R:ϫ���裱��A:�ݸ����
               05  TBL-RSICHG-KBN             PIC X(01).
      *        ��������
               05  TBL-RSICHG-MAE-SRYCD       PIC X(09).
      *
               05  TBL-RSICHG-YOBI1           PIC X(01).
      *        �女����
               05  TBL-RSICHG-ATO-SRYCD       PIC X(09).
      *