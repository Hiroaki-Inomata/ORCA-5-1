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
      ************************************************************
      *    ��������(���饤����Ȱ�����) COPY  (CPPRTCTRL.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  XX.XX.XX    NACL-XXXX    99/99/99 
      ************************************************************
            03  PRTCTRL-KEY.
      *  ���ŵ��أɣ�
              05  PRTCTRL-HOSPNUM      PIC S9(2).
      *  KEY
              05  PRTCTRL-TBL-KEY      PIC X(8).
      *  Ϣ��
              05  PRTCTRL-RENNUM       PIC S9(4).
      *  ���롼��
              05  PRTCTRL-TBL-GROUP    PIC X(14).
      *  UUID
              05  PRTCTRL-TBL-UUID     PIC X(36).
      *  ������Ϣ��
              05  PRTCTRL-SHORI-RENNUM PIC S9(4).
              05  PRTCTRL-PAGE         PIC S9(5).
      *  �ǡ�����ʬ
            03  PRTCTRL-DATA-KBN       PIC S9(2).
      *  Ϣ³�ե饰
            03  PRTCTRL-CONT-FLG       PIC S9(2).
      *  ��̳������
            03  PRTCTRL-GYOUMU-CD      PIC S9(3).
      *  �����ǡ�����Ǽ̾��
            03  PRTCTRL-DNAME          PIC X(200).
      *  ü��ID
            03  PRTCTRL-TERMID         PIC X(64).
            03  PRTCTRL-OPID           PIC X(16).
            03  PRTCTRL-CREYMD         PIC X(8).
            03  PRTCTRL-CREHMS         PIC X(6).
            03  PRTCTRL-SELLIMIT       PIC S9(8).
            03  PRTCTRL-SELOFFSET      PIC S9(8).