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
      *----------------------------------------------------------*
      *    �����ƥ�����쥳��������������֥롼����ѥ�᥿
      *                                   COPY (CPORCSSKDEF.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSSKDEFAREA.
      *    ����������
           03  ORCSSKDEF-KANRICD       PIC X(04).
      *    �쥳����̾��
           03  ORCSSKDEF-NAME          PIC X(40).
      *    ��ʬ������
           03  ORCSSKDEF-KBNCD         PIC X(08).
      *    ���ܿ�
           03  ORCSSKDEF-ITEM-MAX      PIC 9(03).
      *    ���ܥơ��֥�
           03  ORCSSKDEF-ITEM-TBL.
               05  ORCSSKDEF-ITEM-OCC          OCCURS  500.
      *            �����ֹ�
                   07  ORCSSKDEF-ITEM-NO       PIC 9(03).
      *            ����̾
                   07  ORCSSKDEF-ITEM          PIC X(30).
      *            ����̾�ʴ�����
                   07  ORCSSKDEF-ITEM-JP       PIC X(80).
      *            ���ܥ����ס�9:������
      *                        A:�ѻ���
      *                        X:�ѿ�����Ⱦ�ѡˡ�
      *                        N:�ѿ��������ѡˡ�
      *                        M:�ѿ����ʺ��ߡˡ�
                   07  ORCSSKDEF-TYPE          PIC X(01).
      *            ���ܳ��ϰ���
                   07  ORCSSKDEF-START         PIC 9(03).
      *            ����Ĺ�ʺ��祵������
                   07  ORCSSKDEF-LENGTH        PIC 9(03).
      *            ��������
                   07  ORCSSKDEF-PRECISION     PIC 9(01).
      *