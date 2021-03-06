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
      *    �����������     COPY (CPSKT0310TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ���         08/10/09  ����̾���ɲ�
      *  01.00.02    ���         18/10/11  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0310TMP-REC.
      *  KEY
           03  SKT0310TMP-KEY.
      *    ���Ͻ�
               05  SKT0310TMP-SYUTURYOKU           PIC  9(01).
      *    �ݸ����ֹ�
               05  SKT0310TMP-HKNJANUM             PIC  X(06).
      *    ���ܾ���Φ�Ͽ̸���
               05  SKT0310TMP-GENMEN               PIC  X(05).
      *  ����ơ��֥�
           03  SKT0310TMP-SEIKYU-TBL     OCCURS  13.
      *      �����ơ��֥�
               05  SKT0310TMP-NYUGAI-TBL  OCCURS  2.
      *         ���
                   07  SKT0310TMP-KENSU            PIC  9(06).
      *         ����
                   07  SKT0310TMP-NISSU            PIC  9(06).
      *         ����
                   07  SKT0310TMP-TENSU            PIC  9(10).
      *         ���ް�����ô��
                   07  SKT0310TMP-YKZFTN           PIC  9(10).
      *         ������ô���
                   07  SKT0310TMP-ITBFTN           PIC  9(10).
      *         Ĺ��
                   07  SKT0310TMP-TYOKI            PIC  9(02).
      *      ���������
               05  SKT0310TMP-S-KENSU              PIC  9(06).
      *      ����������
               05  SKT0310TMP-S-NISSU              PIC  9(06).
      *      ���������
               05  SKT0310TMP-S-KINGAKU            PIC  9(10).
      *      ������ɸ����ô��
               05  SKT0310TMP-S-FTNGAKU            PIC  9(10).
      *  ����ơ��֥�
           03  SKT0310TMP-ZAI-TBL     OCCURS  6.
      *      ���𡼷��
               05  SKT0310TMP-ZKENSU               PIC  9(06).
      *      ��������
               05  SKT0310TMP-ZNISSU               PIC  9(06).
      *      ��������
               05  SKT0310TMP-ZTENSU               PIC  9(10).
      *      �������ް�����ô��
               05  SKT0310TMP-ZYKZFTN              PIC  9(10).
      *      ���𡼰�����ô���
               05  SKT0310TMP-ZITBFTN              PIC  9(10).
      *      ����Ĺ��
               05  SKT0310TMP-ZTYOKI               PIC  9(02).
      *  ����ơ��֥�
           03  SKT0310TMP-KOH-TBL      OCCURS  20.
      *      �����ֹ�
               05  SKT0310TMP-KOHNUM               PIC  X(03).
               05  SKT0310TMP-KOHNAME              PIC  X(10).
      *      �����ơ��֥�
               05  SKT0310TMP-R-NYUGAI-TBL  OCCURS  2.
      *          ���񡼷��
                   07  SKT0310TMP-KKENSU           PIC  9(06).
      *          ��������
                   07  SKT0310TMP-KNISSU           PIC  9(06).
      *          ��������
                   07  SKT0310TMP-KTENSU           PIC  9(10).
      *          �������ް�����ô��
                   07  SKT0310TMP-KYKZFTN          PIC  9(10).
      *          ���񡼰�����ô���
                   07  SKT0310TMP-KITBFTN          PIC  9(10).
      *      ���񡼿������
               05  SKT0310TMP-S-KKENSU             PIC  9(06).
      *      ���񡼿�������
               05  SKT0310TMP-S-KNISSU             PIC  9(06).
      *      ���񡼿������
               05  SKT0310TMP-S-KKINGAKU           PIC  9(10).
      *      ���񡼿���ɸ����ô��
               05  SKT0310TMP-S-KFTNGAKU           PIC  9(10).
      *  ���ճ��
           03  SKT0310TMP-KYURATE1                 PIC  9(02).
           03  SKT0310TMP-KYURATE2                 PIC  9(02).
