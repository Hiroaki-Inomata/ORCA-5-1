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
      *****************************************************************
      *    ����                COPY (CPADRS.INC)        *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  ADRS-REC.
           03  ADRS-KEY.
               05  ADRS-HOSPNUM        PIC 9(2).
      *�����������Υ�����
               05  ADRS-LPUBCD         PIC X(05).
      *͹���ֹ�
               05  ADRS-POST           PIC X(07).
      *����
               05  ADRS-EDANUM         PIC 9(02).
      *��ƻ�ܸ�̾����
           03  ADRS-PREFKANA           PIC X(40).
      *��Į¼̾����
           03  ADRS-CITYKANA           PIC X(200).
      *Į��̾����(200->400)4.6.0
           03  ADRS-TOWNKANA           PIC X(400).
      *��ƻ�ܸ�̾
           03  ADRS-PREFNAME           PIC X(20).
      *��Į¼̾
           03  ADRS-CITYNAME           PIC X(100).
      *Į��̾(100->200)4.6.0
           03  ADRS-TOWNNAME           PIC X(200).
      *�Խ��彻�ꥫ��(440->640)4.6.0
           03  ADRS-EDITADRS-KANA      PIC X(640).
      *�Խ��彻��(220->320)4.6.0
           03  ADRS-EDITADRS-NAME      PIC X(320).
      *Į��ʬ��ե饰
           03  ADRS-TOWNDIVFLG         PIC X(01).
      *ʣ��Į��ե饰
           03  ADRS-MANYTOWNFLG        PIC X(01).
      *��­���̤�̾��4.6.0
           03  ADRS-TOWNNAME2          PIC X(1200).
      *
