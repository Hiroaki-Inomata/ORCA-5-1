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
      *    ���ౡ�����������ѥѥ�᥿  COPY (CPORCSNTYMD.INC)
      *                                ���ѥץ������ (ORCSNTYMD)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNTYMDAREA.
           03  SNTYMD-IN.
      *        ���ŵ��أɣ�
               05  SNTYMD-HOSPNUM          PIC 9(2).
               05  SNTYMD-PTID             PIC 9(10).
      *        ������ʬ 1...���Ƥ�������
      *                 2...¾��������
      *                 3...¾���򡦼���������
               05  SNTYMD-KBN              PIC X(01).
           03  SNTYMD-OT.
      *        �ֵѷ��
               05  SNTYMD-MAX              PIC 9(05).
               05  SNTYMD-OCC              OCCURS  50.
      *            ���������ֹ�
                   07  SNTYMD-RRKNUM       PIC 9(03).
      *            ������
                   07  SNTYMD-NYUINYMD     PIC X(08).
      *            �ౡ��
                   07  SNTYMD-TAIINYMD     PIC X(08).
      *            ����ֹ�
                   07  SNTYMD-SHONUM       PIC 9(03).
      *            ���ֶ�ʬ
                   07  SNTYMD-JTIKBN       PIC X(01).
      *        �꥿���󥳡���
               05  SNTYMD-RC               PIC 9(02).