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
      *  ������            : ����
      *  Maj/Min/Rev  ������  ����        ����
      *   01.00.00    ����    2011/11/07  ��������
      *   01.00.01    ����    2014/05/22  �������η������
      *   01.00.02    ���    2019/03/14  �¿Ϳ��ڤӱ�ٿͿ��Υ����С��ե����б�
      *******************************************************************
       01  TA01.
      *    ���֥����ȥ�
           03  TA01-SUBTITLE            PIC X(20).
      *    ���֥����ȥ룲
           03  TA01-SUBTITLE2           PIC X(30).
      *    ����ǯ��
           03  TA01-SRYYM               PIC X(16).
      *    ����ǯ����
           03  TA01-CREATEYMD           PIC X(22).
      *    �ڡ�����
           03  TA01-PAGE                PIC X(02).
      *    ��ץڡ�����
           03  TA01-GPAGE               PIC X(02).
      *    �����ʳ����
           03  TA01-NYUGAI              PIC X(06).
      *    ���٥ơ��֥�
           03  TA01-MEISAI-TBL          OCCURS  34 TIMES.
      *    �ݸ����̣�(���ݡ�����)
               05  TA01-HKNSBT1         PIC X(04).
      *    �ݸ����̣�(ñ�ȡ�ʻ��)
               05  TA01-HKNSBT2         PIC X(04).
      *    �ܿͲ�²��ʬ
               05  TA01-HONKZK          PIC X(04).
      *    �����ֹ�(1���10��)
               05  TA01-PTNUM           PIC X(10).
      *    �����ֹ�(11���20��)
               05  TA01-PTNUM2          PIC X(20).
      *    ��̾
               05  TA01-NAME            PIC X(20).
      *    �ݸ����ֹ�
               05  TA01-HKNJANUM        PIC X(08).
      *    ������
               05  TA01-NISSU           PIC X(03).
      *    ����
               05  TA01-TENSU           PIC X(09).
      *    ������ô
               05  TA01-ITBFTN          PIC X(09).
      *    ��������
               05  TA01-SKJ-NISSU       PIC X(03).
      *    �������
               05  TA01-SKJ-RYOYOHI     PIC X(09).
      *    ������ô
               05  TA01-SKJ-FTN         PIC X(09).
      *    ��������
               05  TA01-RMSAGAKU        PIC X(09).
      *    �������
               05  TA01-FURIKOMI        PIC X(09).
      *    �����ˡ��
               05  TA01-KOHNUM          PIC X(03).
      *    ��ô���ֹ�
               05  TA01-FTNJANUM        PIC X(08).
      *    ������ֹ�
               05  TA01-JKSNUM          PIC X(08).
      *    ����ݼ�����
               05  TA01-K-NISSU         PIC X(03).
      *    ���������
               05  TA01-K-TENSU         PIC X(09).
      *    ����ݰ�����ô
               05  TA01-K-ITBFTN        PIC X(09).
      *    ����ݿ������
               05  TA01-K-SKJ-RYOYOHI   PIC X(09).
      *    ����ݿ�����ô
               05  TA01-K-SKJ-FTN       PIC X(09).
      *    ���ŷ�(�����)
               05  TA01-HENREIYM        PIC X(07).
      *    ��ץơ��֥�
           03  TA01-GOKEI-TBL.
      *    ��סݼ¿Ϳ�
               05  TA01-JNINZU          PIC X(04).
      *    ��סݱ�ٿͿ�
               05  TA01-NNINZU          PIC X(04).
      *    ��סݼ�����
               05  TA01-GNISSU          PIC X(04).
      *    ��ס�����
               05  TA01-GTENSU          PIC X(10).
      *    ��סݰ�����ô
               05  TA01-GITBFTN         PIC X(10).
      *    ��סݿ�������
               05  TA01-GSKJ-NISSU      PIC X(04).
      *    ��סݿ������
               05  TA01-GSKJ-RYOYOHI    PIC X(10).
      *    ��סݿ�����ô
               05  TA01-GSKJ-FTN        PIC X(10).
      *    ��סݼ�������
               05  TA01-GRMSAGAKU       PIC X(10).
      *    ��סݿ������
               05  TA01-GFURIKOMI       PIC X(10).
      *    ��סݸ���ݼ�����
               05  TA01-GK-NISSU        PIC X(04).
      *    ��סݸ��������
               05  TA01-GK-TENSU        PIC X(10).
      *    ��סݸ���ݰ�����ô
               05  TA01-GK-ITBFTN       PIC X(10).
      *    ��סݸ���ݿ������
               05  TA01-GK-SKJ-RYOYOHI  PIC X(10).
      *    ��סݸ���ݿ�����ô
               05  TA01-GK-SKJ-FTN      PIC X(10).
      *    ��סݼ¿Ϳ���
               05  TA01-JNINZU2         PIC X(05).
      *    ��סݱ�ٿͿ���
               05  TA01-NNINZU2         PIC X(05).
