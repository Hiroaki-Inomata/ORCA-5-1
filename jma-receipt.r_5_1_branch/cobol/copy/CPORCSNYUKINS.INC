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
      *    �ޤȤ������Խ��ѥ�᥿����̤���ޤȤ���������ѡ�      *
      *                         COPY  (CPORCSNYUKINS.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNYUKINAREAS.
      *������
      *    ���ŵ��أɣġ�ɬ�ܡ�
           03  LNK-SCNYUKINS-HOSPNUM         PIC 9(2).
      *    ���ԣɣġ�ɬ�ܡ�
           03  LNK-SCNYUKINS-PTID            PIC 9(10).
      *    ������
           03  LNK-SCNYUKINS-NYUKIN-YMD      PIC X(08).
      *    ������ˡ
           03  LNK-SCNYUKINS-NYUKIN-HOHO     PIC X(02).
      *    ������������
           03  LNK-SCNYUKINS-AREA.
               05  LNK-SCNYUKINS-MAX         PIC 9(05).
               05  LNK-SCNYUKINS-TBL    OCCURS 500.
      *            ������ʬ
                   07  LNK-SCNYUKINS-NYUGAIKBN     PIC X(01).
      *            ��ɼ�ֹ�
                   07  LNK-SCNYUKINS-DENPNUM       PIC 9(07).
      *            ��������
                   07  LNK-SCNYUKINS-NYUKIN-YMD2   PIC X(08).
      *            �������
                   07  LNK-SCNYUKINS-NYUKIN-HMS2   PIC X(06).
      *            ������ˡ
                   07  LNK-SCNYUKINS-NYUKIN-HOHO2  PIC X(02).
      *            �����
                   07  LNK-SCNYUKINS-NYUKIN-MONEY  PIC S9(08).
      *
      *            �ʲ��ֵѾ���
                   07  LNK-SCNYUKINS-SKYMONEY      PIC S9(08).
                   07  LNK-SCNYUKINS-NYUKIN-TOTAL  PIC S9(08).
                   07  LNK-SCNYUKINS-DENPJTIKBN    PIC X(01).
                   07  LNK-SCNYUKINS-CREATEKBN     PIC X(01).
                   07  LNK-SCNYUKINS-DENPEDANUM    PIC 9(02).
      *
      *����������
      *00:����
      *10:�ѥ�᡼������
      *11:��Ǽ�������顼
      *12:��Ǽ���٥��󥵡��ȥ��顼
           03  LNK-SCNYUKINS-RC              PIC 9(02).
      *
      *