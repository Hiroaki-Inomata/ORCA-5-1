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
      *    ������                COPY (CPADRSWK.INC)        *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  ADRSWK-REC.
           03  ADRSWK-SYORIKBN         PIC X(01).
           03  ADRSWK-KEY.
      *�����������Υ�����
               05  ADRSWK-LPUBCD       PIC X(05).
      *͹���ֹ�
               05  ADRSWK-POST         PIC X(07).
      *����
               05  ADRSWK-EDANUM       PIC 9(02).
      *��ƻ�ܸ�̾����
           03  ADRSWK-PREFKANA         PIC X(40).
      *��Į¼̾����
           03  ADRSWK-CITYKANA         PIC X(200).
      *Į��̾����
           03  ADRSWK-TOWNKANA         PIC X(200).
      *��ƻ�ܸ�̾
           03  ADRSWK-PREFNAME         PIC X(20).
      *��Į¼̾
           03  ADRSWK-CITYNAME         PIC X(100).
      *Į��̾
           03  ADRSWK-TOWNNAME         PIC X(100).
      *�Խ��彻�ꥫ��
           03  ADRSWK-EDITADRS-KANA    PIC X(440).
      *�Խ��彻��
           03  ADRSWK-EDITADRS-NAME    PIC X(220).
      *Į��ʬ��ե饰
           03  ADRSWK-TOWNDIVFLG       PIC X(01).
      *ʣ��Į��ե饰
           03  ADRSWK-MANYTOWNFLG      PIC X(01).
      *
