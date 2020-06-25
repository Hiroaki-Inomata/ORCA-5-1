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
      *    ���ݸ�����������å�ɽ��֥ե�����                    *
      *    COPY  (BGSF01.INC)                                  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  BGSF01-REC.
           03  BGSF01-KEY.
      *    �ݸ�
      *      1:����/2:����/3:��������/4:����������/5:����/
      *      6:����/7:ϫ��/8:������/9:�軰�Թ԰�/10:����
               05  BGSF01-HOKEN        PIC 9(02).
      *    �������
      *    0:����ʤ�(�̽��פ����硢1��3������)
      *    1:�̾�����/2:���٤�/3:����
               05  BGSF01-SKYSBT       PIC X(01).
      *    ������ʬ
               05  BGSF01-NYUGAIKBN    PIC X(01).
      *    ���Ų�
               05  BGSF01-RECEKA       PIC X(02).
      *    �쥻�ץȼ���(1,4) ��()�ο����Ϲ��ܤ���Ѥ���ǡ�����ʬ
      *    �쥻�ץ��̤��ݸ���ʬ������
      *      1:����/2:����/3:�ݸ���/4:�쥻�ץ���
               05  BGSF01-RECESBT-G.
                   07  BGSF01-RECESBT  PIC X(04).
                   07  BGSF01-FIL1     PIC X(02).
               05  BGSF01-SBTKBN-G     REDEFINES BGSF01-RECESBT-G.
                   07  BGSF01-SBTGRPID PIC 9(02).
                   07  BGSF01-SBTRATE  PIC 9(02).
                   07  BGSF01-SBTHKN   PIC 9(02).
      *    ������ô����H26.5�ʹߥ쥻�ץ��̤���������ԤΤ������
               05  BGSF01-FTNRATE      PIC 9(02).
      *    �ݸ��ֹ�(1)
               05  BGSF01-HKNNUM       PIC X(03).
      *    �����ʬ    (2)
      *      1:�쥻�ץ�����ʬ/2:�������������ʬ
               05  BGSF01-KOHKBN       PIC X(01).
      *    ����μ���  (2)
               05  BGSF01-KOHNUM       PIC X(03).
      *    ���⡦������ʬ (3)
               05  BGSF01-PREFKBN      PIC X(01).
      *    �ݸ����ֹ�  (3)
               05  BGSF01-HKNJANUM     PIC X(08).
      *    ���롼�ףɣ�
               05  BGSF01-GROUPID      PIC 9(05).
      *    ����ǯ��(�쥻�ץ��̤ǻ���)
               05  BGSF01-SRYYM        PIC X(06).
      *    ���ʻ�̾(�쥻�ץ��̤ǻ���)
               05  BGSF01-KANANAME     PIC X(50).
      *    ��̾(�쥻�ץ��̤ǻ���)
               05  BGSF01-NAME         PIC X(50).
      *    ����ID(�쥻�ץ��̤ǻ���)
               05  BGSF01-PTID         PIC 9(10).
      *    �쥻�ץȼ���(�쥻�ץ��̤������
               05  BGSF01-RECESBT2     PIC X(04).
      *    Ϣ��(�쥻�ץ��̤������
               05  BGSF01-SEQ          PIC 9(10).
      *    ���
           03  BGSF01-KENSU            PIC 9(06).
      *    ������
           03  BGSF01-NISSU            PIC 9(08).
      *    ������
           03  BGSF01-TENSU            PIC 9(08).
      *    ������ô
           03  BGSF01-FTNMONEY         PIC 9(09).
      *    �ݸ�����ô
           03  BGSF01-HKNMONEY         PIC 9(09).
      *    ������ô
           03  BGSF01-KOHMONEY         PIC 9(09).
      *    ���(������ô+�ݸ�����ô+������ô)
           03  BGSF01-TTLMONEY         PIC 9(09).
      *    �������
           03  BGSF01-SKJKAISU         PIC 9(06).
      *    ���������
           03  BGSF01-SKJTTLMONEY      PIC 9(09).
      *    ����������ô
           03  BGSF01-SKJFTNMONEY      PIC 9(09).
      *    �����ݸ�����ô
           03  BGSF01-SKJHKNMONEY      PIC 9(09).
      *    ����������ô
           03  BGSF01-SKJKOHMONEY      PIC 9(09).
      *    ��ף��ʹ�סܿ�������ߡ�
           03  BGSF01-TTLTTLMONEY      PIC 9(09).
      *    ����ե饰�ʥ쥻�ץȤȸ�������������ǯ��ۤʤ����
      *   �ʣ���Ʊ�����ϸ��������ʤ��������쥻�ץȤΤߡ��������������Τߡ�
           03  BGSF01-SEIKYUFLG        PIC 9(01).
      *
      *    �ʲ�����¾����ǻ���
      *    ����
           03  BGSF01-JIHIMONEY        PIC S9(09).
      *    ����
           03  BGSF01-GENMEN           PIC S9(09).
      *    Ĵ����
           03  BGSF01-CHOSEI1          PIC S9(09).
      *    Ĵ����
           03  BGSF01-CHOSEI2          PIC S9(09).
      *    ü��Ĵ����
           03  BGSF01-HASU             PIC S9(09).