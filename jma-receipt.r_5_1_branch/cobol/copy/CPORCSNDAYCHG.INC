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
      *    ���Ź԰ٻ�����������ϥѥ�᥿  COPY (CPORCSNDAYCHG.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSNDAYCHGAREA.
      *    ��������
           03  ORCSNDAYCHG-INPUTCD          PIC X(100).
      *    ����ǯ����
           03  ORCSNDAYCHG-SRYYMD           PIC X(08).
      *    ���Ϸ��
           03  ORCSNDAYCHG-KETA             PIC 9(03).
      *    ������ʬ�ʣ����������֥����å������
           03  ORCSNDAYCHG-KBN              PIC X(01).
      *    ��������
           03  ORCSNDAYCHG-NYUINYMD         PIC X(08).
           03  ORCSNDAYCHG-TAIINYMD         PIC X(08).
      *
      *    �����ΰ�
           03  ORCSNDAYCHG-OUTAREA.
      *        ���
               05  ORCSNDAYCHG-KAISU        PIC 9(03).
      *        ������
               05  ORCSNDAYCHG-KAISUCNT     PIC 9(03).
      *        �������
               05  ORCSNDAYCHG-NYUIN-KAISU.
                   07  ORCSNDAYCHG-NYU-DAY      PIC 9(02)
                                                OCCURS   31.
      *        ���顼��ʬ
               05  ORCSNDAYCHG-ERRCD        PIC X(04).
      *        ���顼����
               05  ORCSNDAYCHG-ERRMSG       PIC X(100).
      *