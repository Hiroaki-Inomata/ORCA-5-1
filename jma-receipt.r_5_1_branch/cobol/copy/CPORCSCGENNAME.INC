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
      *    �����������̾�Խ��ѥ�᥿����COPY (CPORCSCGENNAME.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCGENNAMEAREA.
      *
      *����
      *������ʬ�ʣ�������Ƚ��ʤ���
           03  ORCSCGENN-SYORIKBN        PIC  X(01).
      *������ʬ���ʣ����ޥ��������ʤ���
           03  ORCSCGENN-SYORIKBN2       PIC  X(01).
      *��������
           03  ORCSCGENN-SRYYMD          PIC  X(08).
      *��ȯ�����ʤؤ��ѹ��Ľ�̾(SYS-1030-KOUHATUKA)
           03  ORCSCGENN-1030-KOUHATUKA  PIC  X(01).
      *��ȯ���ѹ��Բġʽ������ͭ
           03  ORCSCGENN-KOUHATU-IN      PIC  X(01).
      *���ż���
           03  ORCSCGENN-SRYKBN          PIC  X(02).
           03  ORCSCGENN-SRYSYUKBN       PIC  X(03).
      *������
           03  ORCSCGENN-ZAITEN          PIC  9(07).
      *
      *������
           03  ORCSCGENN-DATA-AREA.
      *�ͣ���
               05  ORCSCGENN-TBLMAX         PIC  9(03).
               05  ORCSCGENN-DATA-TBL       OCCURS   51.
      *IN
      *            ���ť�����
                   07  ORCSCGENN-SRYCD          PIC X(09).
      *            ���ť�����(����ɽ����
                   07  ORCSCGENN-INPUTCD        PIC X(20).
      *
      *            �����ޥ����ʸ�ȯ�ʶ�ʬ��
                   07  ORCSCGENN-TNS-KOUHATUKBN     PIC 9(01).
      *            �����ޥ����ղáʰ���̾���ܶ�ʬ��
                   07  ORCSCGENN-IPN-KISAIKBN       PIC 9(01).
      *
      *            ����̾�����ɡ�������ॳ���ɡ�
                   07  ORCSGETGE-YAKKAKJNCD     PIC X(12).
      *            ����̾��
                   07  ORCSCGENN-GENERICNAME    PIC X(200).
      *OUT
      *            ��������̾�Ρ��ʽ�����ʬ���ᣱ���Ϥʤ���
                   07  ORCSCGENN-HEN-NAME       PIC X(100).
      *            ̾�ζ�ʬ��1:����̾,2������̾�Ρ�3������̾�ʰ��̡�
                   07  ORCSCGENN-NAME-KBN       PIC X(01).
      *            ��������ñ��̾��
                   07  ORCSCGENN-HEN-TANINAME   PIC X(24).
      *
      *
           03  ORCSCGENN-OUT-AREA.
      *        ���⡦������ʬ�ʣ������⡢����������
               05  ORCSCGENN-INNAIKBN        PIC X(01).
      *        ���顼������
               05  ORCSCGENN-ERRCD           PIC 9(01).
      *