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
      *    ���У����ӽ�λ�ǡ����Խ����֥ѥ�᥿
      *                                           (CPORAPISUBWKSRY.INC)
      ******************************************************************
       01  ORAPISUBWKSRYAREA.
      *    ������ʬ
           03  ORAPIWKSRY-SYORIKBN     PIC X(01).
      *    ���ֳ��û���ʬ
           03  ORAPIWKSRY-TIMEFLG      PIC X(01).
      *    �ݸ�����
           03  ORAPIWKSRY-HKNCOMBI-AREA.
      *        �ݸ��ȹ礻�ֹ�
               05  ORAPIWKSRY-HKNCOMBI     PIC X(04).
      *
      *    ��������
           03  ORAPIWKSRY-SRYACT-AREA.
               05  ORAPIWKSRY-MDC-INFO         OCCURS  40 TIMES.
      *            ���ż��̶�ʬ
                   07  ORAPIWKSRY-MDC-CLASS        PIC X(3).
      *            ���Ŷ�ʬ̾��
                   07  ORAPIWKSRY-MDC-CLASS-NAME   PIC X(50).
      *            ���
                   07  ORAPIWKSRY-MDC-CLASS-NUMBER PIC X(50).
      *            ������
                   07  ORAPIWKSRY-PRESCRIPTION-INFO    OCCURS  40 TIMES.
      *                ���ť�����
                       09  ORAPIWKSRY-PRSCRPT-CODE     PIC X(9).
      *                ����̾�Ρʥ����ȡ�
                       09  ORAPIWKSRY-PRSCRPT-NAME     PIC X(140).
      *                ����
                       09  ORAPIWKSRY-PRSCRPT-NUMBER   PIC X(40).
      *                �����������̾����
                       09  ORAPIWKSRY-GENERIC-FLG      PIC X(03).
      *
      *    ���顼��ʬ
           03  ORAPIWKSRY-ERRCD        PIC X(04).
           03  ORAPIWKSRY-ERRMSG       PIC X(100).
      *
