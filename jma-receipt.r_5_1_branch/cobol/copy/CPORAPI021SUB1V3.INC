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
      * 04.08.00     NACL-¿��Ǽ  15/11/XX  ���顼���ť������ɲ�
      * 05.00.00     NACL-¿��Ǽ  17/11/XX  �����б��ɲ�
      * 05.00.00     NACL-¿��Ǽ  19/01/XX  ʣ�����ݸ��б�
      *****************************************************************
      ******************************************************************
      *    ���У����ӽ�λ�ǡ����Խ����֥ѥ�᥿�ʰ��β���
      *                                         (CPORAPI021SUB1V3.INC)
      ******************************************************************
       01  ORAPI021SUB1V3AREA.
      *    ������ʬ
           03  ORAPISUB01-SYORIKBN      PIC X(03).
      *    ���ֳ��û���ʬ
           03  ORAPISUB01-TIMEFLG       PIC X(01).
      *    �ݸ�����
           03  ORAPISUB01-HKNCOMBI-AREA.
      *        �ݸ��ȹ礻�ֹ�
               05  ORAPISUB01-HKNCOMBI     PIC X(04).
      *
      *    ��������
           03  ORAPISUB01-SRYACT-AREA.
               05  ORAPISUB01-MDC-INFO         OCCURS  110 TIMES.
      *!!
      *H31.01
      *            �ǡ�����ʬ��DEP��ʣ�����Ųʡ�COM:�ݸ��ȹ礻�ֹ�Υ쥳���ɡ�
                   07  ORAPISUB01-MDC-DATA-KBN         PIC X(03).
      *            �ݸ��ȹ礻�ֹ�
                   07  ORAPISUB01-MDC-COMBINATION      PIC X(04).
      *            ���Ų�
                   07  ORAPISUB01-MDC-DEP-CODE         PIC X(02).
      *            �ɥ�����
                   07  ORAPISUB01-MDC-DR-CODE          PIC X(05).
      *!!
      *            ���ż��̶�ʬ
                   07  ORAPISUB01-MDC-CLASS-KBN    PIC X(2).
      *            ���ż��̶�ʬ
                   07  ORAPISUB01-MDC-CLASS        PIC X(3).
      *            ���Ŷ�ʬ̾��
                   07  ORAPISUB01-MDC-CLASS-NAME   PIC X(40).
      *            ���
                   07  ORAPISUB01-MDC-CLASS-NUMBER PIC X(3).
      *H29.12 ������
      *            ������
                   07  ORAPISUB01-MDC-DAY-G.
                       09  ORAPISUB01-MDC-DAY      PIC X(01)
                                                   OCCURS   31.
      *            ������
                   07  ORAPISUB01-PRESCRIPTION-INFO    OCCURS  50 TIMES.
      *                ���ť�����
                       09  ORAPISUB01-PRSCRPT-CODE     PIC X(9).
      *                ����̾�Ρʥ����ȡ�
                       09  ORAPISUB01-PRSCRPT-NAME     PIC X(140).
      *                ����
                       09  ORAPISUB01-PRSCRPT-NUMBER   PIC X(11).
      *                ������
                       09  ORAPISUB01-PRSCRPT-MONEY    PIC X(07).
      *                ��������������
                       09  ORAPISUB01-ATAI-G.
                           11  ORAPISUB01-ATAI         PIC X(10)
                                                       OCCURS   5.
      *H29.5
      *                �ե����ʬ���
                       09  ORAPISUB01-PRSCRPT-FILMNUM  PIC X(03).
      *                ��³�ؼ���ʬ
                       09  ORAPISUB01-CONTKBN          PIC X(01).
      *                ����������ؼ���ʬ
                       09  ORAPISUB01-NAIFKBN          PIC X(01).
      *H29.5
      *                ��ư����ʤ���ʬ�ʺ������ܼ��Ӳû���
                       09  ORAPISUB01-NOAUTO           PIC X(03).
      *
      *                ��ư��ʬ
                       09  ORAPISUB01-AUTOKBN          PIC X(01).
      *
      *    ���顼��ʬ
           03  ORAPISUB01-ERRCD        PIC X(02).
           03  ORAPISUB01-ERRMSG       PIC X(100).
      *    ��������Υ��顼
           03  ORAPISUB01-INP-ERR-G.
               05  ORAPISUB01-IN-ERR-TBL   OCCURS   200.
                   07  ORAPISUB01-IN-ERR       PIC X(03).
                   07  ORAPISUB01-IN-ERRMSG    PIC X(100).
      *            ���顼����
                   07  ORAPISUB01-IN-POS       PIC 9(03).
      *            ���顼����
                   07  ORAPISUB01-IN-LINE      PIC 9(02).
      *            ���顼���ť�����
                   07  ORAPISUB01-IN-SRYCD     PIC X(09).
      *