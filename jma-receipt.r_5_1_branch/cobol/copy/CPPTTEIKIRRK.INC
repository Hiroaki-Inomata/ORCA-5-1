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
      *    ���������������                   (CPPTTEIKIRRK.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTTEIKIRRK-REC.
      *���ŵ��أɣ�
           03  PTTEIKIRRK-HOSPNUM           PIC 9(2).
      *���ԣɣ�
           03  PTTEIKIRRK-PTID             PIC  9(10).
      *����ǯ��
           03  PTTEIKIRRK-SRYYM            PIC  X(06).
      *������ʬ�ʽ�������
      *    ���������ƽ������������������Τߡ����ౡ��Ͽ�ǻ����
           03  PTTEIKIRRK-SAKKBN           PIC  X(01).
      *�ݸ��ȹ礻
           03  PTTEIKIRRK-HKNCOMBINUM      PIC  9(04).
      *��Ǽ����ɼ�ֹ�
           03  PTTEIKIRRK-DENPNUM          PIC  9(07).
      *����ե饰
           03  PTTEIKIRRK-DELFLG           PIC  9(01).
      *������ʬ
           03  PTTEIKIRRK-SYOKBN           PIC  X(01).
      *����
           03  PTTEIKIRRK-KBT              PIC  X(01).
      *���ᳫ��ǯ����
           03  PTTEIKIRRK-SKYSTYMD         PIC  X(08).
      *���Ὢλǯ����
           03  PTTEIKIRRK-SKYEDYMD         PIC  X(08).
      *��������
           03  PTTEIKIRRK-SKYTEN           PIC  S9(09).
      *�����ۡݾ����ǡʺƷǡ�
           03  PTTEIKIRRK-SKYMONEY-TAX-SAI PIC  S9(07).
      *������
           03  PTTEIKIRRK-SKYMONEY         PIC  S9(09).
      *�������������Ϣ��
           03  PTTEIKIRRK-RRKNUM           PIC  9(03).
      *��Ǽ����ɼ�ֹ����
           03  PTTEIKIRRK-DENPEDANUM       PIC  9(02).
      *����ǡ��������ե饰�ʣ�������������������
           03  PTTEIKIRRK-SKYUPDFLG        PIC  9(01).
      *ü���ɣ�
           03  PTTEIKIRRK-TERMID           PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTTEIKIRRK-OPID             PIC  X(16).
      *����ǯ����
           03  PTTEIKIRRK-CREYMD           PIC  X(08).
      *����ǯ����
           03  PTTEIKIRRK-UPYMD            PIC  X(08).
      *��������
           03  PTTEIKIRRK-UPHMS            PIC  X(06).
