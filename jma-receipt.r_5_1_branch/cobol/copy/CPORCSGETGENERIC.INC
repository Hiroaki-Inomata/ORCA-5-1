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
      *    ������������̾�Խ��ѥ�᥿����COPY (CPORCSGETGERIC.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSGETGENERICAREA.
      *
      *����
           03  ORCSGETGE-IN-AREA.
      *���ŵ��أɣ�
               05  ORCSGETGE-HOSPNUM         PIC  9(02).
      *���ԣɣ�
               05  ORCSGETGE-PTID            PIC  9(10).
      *����ǯ��
               05  ORCSGETGE-SRYYM           PIC  X(06).
      *������ʬ
               05  ORCSGETGE-NYUGAIKBN       PIC  X(01).
      *���ֹ�
               05  ORCSGETGE-ZAINUM          PIC  9(08).
      *
      *��ȯ�����ʤؤ��ѹ��Ľ�̾(SYS-1030-KOUHATUKA)
               05  ORCSGETGE-KOUHATUKA       PIC  X(01).
      *����
           03  ORCSGETGE-OUT-AREA.
      *�ͣ���
               05  ORCSGETGE-TBLMAX         PIC  9(03).
               05  ORCSGETGE-SRYCD-G        OCCURS   50.
      *            ���ť�����
                   07  ORCSGETGE-SRYCD          PIC X(09).
      *            ����̾�����ɡ�������ॳ���ɡ�
                   07  ORCSGETGE-YAKKAKJNCD     PIC X(12).
      *            ����̾��ʬ�ʣ�������̾�û���
                   07  ORCSGETGE-GENERIC-KBN    PIC X(01).
      *            ����̾��
                   07  ORCSGETGE-GENERICNAME    PIC X(200).
      *            ����̾̾�ζ�ʬ��1:����̾,2������̾�Ρ�
                   07  ORCSGETGE-NAME-KBN       PIC X(01).
      *            ����̾ñ�̥�����
                   07  ORCSGETGE-GEN-TANICD     PIC 9(03).
      *            ����̾ñ��̾��
                   07  ORCSGETGE-GEN-TANINAME   PIC X(24).
      *            ������
                   07  ORCSGETGE-KANZANCHI      PIC 9(05)V9(05).
      *            �����ͤ�ñ��
                   07  ORCSGETGE-KNS-TANICD     PIC 9(03).
      *            �����ͤ�ñ��
                   07  ORCSGETGE-KNS-TANINAME   PIC X(24).
      *        ��ȯ���ѹ��Բġʽ����ˤ���
               05  ORCSGETGE-GENEFLG            PIC X(01).
      *���顼������
           03  ORCSGETGE-ERRCD           PIC 9(01).
      *
      *