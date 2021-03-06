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
      *    ���Уɣ֣����ݸ��ȹ礻�Խ��ѥ�᥿
      *                                         (CPORAPI012SUB2V3.INC)
      ******************************************************************
       01  ORAPI012SUB2V3AREA.
      *
      *    ������ʬ�ʣ�����Ͽ���ơ����������å���
           03  AP12SUB2V3-SYORIKBN     PIC X(01).
      *    �����ƥ�����
           03  AP12SUB2V3-SYSYMD       PIC X(08).
      *
      *    �ݸ��ȹ礻����
           03  AP12SUB2V3-COMBINATION-INF-G.
               05  AP12SUB2V3-COMBINATION-INF   OCCURS 200 TIMES.
      *            ������ʬ�ʿ���������Τߡ�
                   07  AP12SUB2V3-COMB-MODE              PIC X(06).
      *            �ݸ��ȹ礻�ֹ�
                   07  AP12SUB2V3-COMBINATION-NUMBER     PIC X(04).
      *            �ݸ��ȹ礻����ô����������
                   07  AP12SUB2V3-COMBINATION-RATE-ADM   PIC X(04).
      *            �ݸ��ȹ礻����ô���ʳ����
                   07  AP12SUB2V3-COMBINATION-RATE-OUT   PIC X(04).
      *            �ݸ��ȹ礻��ͭ��������
                   07  AP12SUB2V3-COMB-START-DATE        PIC X(10).
      *            �ݸ��ȹ礻��ͭ����λ��
                   07  AP12SUB2V3-COMB-END-DATE          PIC X(10).
      *
      *            �ݸ��ɣ�
                   07  AP12SUB2V3-COMB-MAIN-ID           PIC X(10).
      *            �ݸ��μ���
                   07  AP12SUB2V3-COMB-MAIN-CLASS        PIC X(03).
      *            �ݸ����ֹ�
                   07  AP12SUB2V3-COMB-MAIN-NUMBER       PIC X(08).
      *            �ݸ��μ���̾��
                   07  AP12SUB2V3-COMB-MAIN-NAME         PIC X(20).
      *            ����
                   07  AP12SUB2V3-COMB-MARK              PIC X(80).
      *            �ֹ�
                   07  AP12SUB2V3-COMB-NUMBER            PIC X(80).
      *            ����
                   07  AP12SUB2V3-COMB-EDABAN            PIC X(02).
      *            ��³��ʬ
                   07  AP12SUB2V3-COMB-CONTINUATION       PIC X(01).
      *            �����ʬ
                   07  AP12SUB2V3-COMB-ASSISTANCE         PIC X(01).
      *            �����ʬ̾��
                   07  AP12SUB2V3-COMB-ASSI-NAME          PIC X(20).
      *            �ܿͲ�²��ʬ
                   07  AP12SUB2V3-COMB-FAMILY             PIC X(01).
      *            �������
                   07  AP12SUB2V3-COMB-PUBLIC-INS-INF
                                              OCCURS  4 TIMES.
      *                ����ɣ�
                       09  AP12SUB2V3-INSURANCE-ID         PIC X(10).
      *                ����μ���
                       09  AP12SUB2V3-INSURANCE-CLASS      PIC X(03).
      *                ����μ���̾��
                       09  AP12SUB2V3-INSURANCE-NAME       PIC X(20).
      *                ��ô���ֹ�
                       09  AP12SUB2V3-PROVIDER-NUMBER      PIC X(08).
      *                ������ֹ�
                       09  AP12SUB2V3-RECIPIENT-NUMBER     PIC X(20).
      *                ��������ôΨ�ʳ��
                       09  AP12SUB2V3-RATE-ADMISSION       PIC X(04).
      *                �����ݸ����
                       09  AP12SUB2V3-MONEY-ADMISSION      PIC X(06).
      *                �������ôΨ�ʳ��
                       09  AP12SUB2V3-RATE-OUTPATIENT      PIC X(04).
      *                ����ݸ����
                       09  AP12SUB2V3-MONEY-OUTPATIENT     PIC X(06).
      *
      *    ���
           03  AP12SUB2V3-MAX              PIC 9(04).
      *    ���顼������
           03  AP12SUB2V3-ERRCD            PIC X(02).
      *
