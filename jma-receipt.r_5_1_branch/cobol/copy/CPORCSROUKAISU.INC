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
      *******************************************************************
      ************************************************************
      *    ϫ�ҥ쥻�ץȲ�������ѥѥ�᥿�� COPY (CPORCSROUKAISU.INC)
      *                          ���ѥץ������  (ORCSROUKAISU)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  LNK-ORCSROUKAISU-REC.
      *
      *���ŵ��أɣ�
           03  LNK-ORCSROUKAISU-HOSPNUM         PIC 9(2).
      *������ʬ
           03  LNK-ORCSROUKAISU-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
           03  LNK-ORCSROUKAISU-PTID            PIC  9(10).
      *�ݸ��ȹ礻
           03  LNK-ORCSROUKAISU-HKNCOMBI        PIC  9(04).
      *���ŷ�
           03  LNK-ORCSROUKAISU-SRYYM           PIC  9(06).
      *���ܳ���ǯ����
           03  LNK-ORCSROUKAISU-RYOSTYMD        PIC  X(08).
      *ϫ�ҥ쥻������ܡ����ǯ��
           03  LNK-ORCSROUKAISU-KIJYUNYM-KSI    PIC  X(06).
      *ϫ�ҥ쥻������ܡ����
           03  LNK-ORCSROUKAISU-KAISU-KSI       PIC  9(03).
      *���������ܳ�����
           03  LNK-ORCSROUKAISU-KAISHIBI        PIC  9(02).
      *���Ųʡʳ�����Τߥ��åȡ�
           03  LNK-ORCSROUKAISU-SRYKA           PIC  X(02).
           03  LNK-ORCSROUKAISU-RECESRYKA       PIC  X(02).
      *
      *�ֵ���
      *
           03  LNK-ORCSROUKAISU-OUT-AREA.
      *���
             05  LNK-ORCSROUKAISU-RECEKAISU     PIC  9(03).
      *
      *����������
      *
           03  LNK-ORCSROUKAISU-RC              PIC  9(02).
      *