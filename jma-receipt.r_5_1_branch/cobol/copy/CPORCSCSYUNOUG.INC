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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���У����ӽ�λ�ǡ��������Ǽ�Խ����֥ѥ�᥿�ʰ��β���
      *                                         (CPORCSCSYUNOUG.INC)
      ******************************************************************
       01  ORCSCSYUNOUGAREA.
      *    ������ʬ
           03  ORCSCSYU-SYORIKBN       PIC X(01).
      *    ������ʬ
           03  ORCSCSYU-SYORIPG        PIC X(04).
      *    ���ӥǡ������
           03  ORCSCSYU-WKSRYACT-MAX   PIC 9(04).
      *    ��Ǽ�ǡ������
           03  ORCSCSYU-SYUNOU-MAX     PIC 9(04).
      *    �ݸ��ȹ礻�����Ųʾ���
           03  ORCSCSYU-HKNCOMBI-AREA.
      *        �ݸ��ȹ礻�ֹ�
               05  ORCSCSYU-HKNCOMBI-TBL     OCCURS  15.
                   07  ORCSCSYU-HKNCOMBI     PIC X(04).
                   07  ORCSCSYU-SRYKA        PIC X(02)
                                             OCCURS   10.
      *
      *    ���顼��ʬ
           03  ORCSCSYU-ERRCD        PIC 9(02).
      *