      ******************************************************************
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
      ******************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ����ɽ������
      *  ������            : 
      *  Maj/Min/Rev  ������  ����       ����
      *   01.00.01    ����    11/11/07   ��������
      *******************************************************************
       01  TA01TMP-REC.
      *    ��������
           03  TA01TMP-KEY.
      *    ����ǯ��
               05  TA01TMP-SRYYM             PIC X(06).
      *    ����ʬ
               05  TA01TMP-PREFKBN           PIC X(01).
      *    ������ʬ
               05  TA01TMP-NYUGAIKBN         PIC X(01).
      *    �ݸ�����
               05  TA01TMP-HKNSBT-KEY.
      *    �ݸ����̣�(���ݡ�����)
                   07  TA01TMP-HKNSBT1       PIC X(01).
      *    �ݸ����̣�(ñ�ȡ�ʻ��)
                   07  TA01TMP-HKNSBT2       PIC X(01).
      *    �ܿͲ�²��ʬ
                   07  TA01TMP-HONKZK        PIC X(01).
      *    �����ֹ�
               05  TA01TMP-PTNUM             PIC X(20).
      *    �ݸ����ֹ�
               05  TA01TMP-HKNJANUM          PIC X(08).
      *    ���ŷ�(�����)
               05  TA01TMP-HENREIYM          PIC X(06).
      *    �ǡ����ơ��֥�
           03  TA01TMP-DATA-TBL.
      *    ��̾
               05  TA01TMP-NAME              PIC X(20).
      *    ������
               05  TA01TMP-NISSU             PIC 9(05).
      *    ����
               05  TA01TMP-TENSU             PIC 9(09).
      *    ������ô
               05  TA01TMP-ITBFTN            PIC 9(09).
      *    ��������
               05  TA01TMP-SKJ-NISSU         PIC 9(05).
      *    �������
               05  TA01TMP-SKJ-RYOYOHI       PIC 9(09).
      *    ������ô
               05  TA01TMP-SKJ-FTN           PIC 9(09).
      *    ��������
               05  TA01TMP-RMSAGAKU          PIC 9(09).
      *    �������
               05  TA01TMP-FURIKOMI          PIC 9(09).
      *    ���Ų�
               05  TA01TMP-SRYKA             PIC X(02).
      *    ����ơ��֥�
               05  TA01TMP-KOH-TBL  OCCURS  4.
      *    �����ˡ��
                   07  TA01TMP-KOHNUM        PIC X(03).
      *    �������ô���ֹ�
                   07  TA01TMP-FTNJANUM      PIC X(08).
      *    ����ݼ�����ֹ�
                   07  TA01TMP-JKYSNUM       PIC X(08).
      *    ����ݼ�����
                   07  TA01TMP-K-NISSU       PIC 9(05).
      *    ���������
                   07  TA01TMP-K-TENSU       PIC 9(09).
      *    ����ݰ�����ô
                   07  TA01TMP-K-ITBFTN      PIC 9(09).
      *    ����ݿ������
                   07  TA01TMP-K-SKJ-RYOYOHI PIC 9(09).
      *    ����ݿ�����ô
                   07  TA01TMP-K-SKJ-FTN     PIC 9(09).
