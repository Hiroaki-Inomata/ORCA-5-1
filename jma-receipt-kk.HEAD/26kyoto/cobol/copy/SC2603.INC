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
      *  ���ԡ��罤������
      * Maj/Min/Rev       ������    ����          ����
      * 01.00.01          ����      09/09/02      Ģɼ�ͼ����ѹ���ȼ������
      * 01.00.02          ����      14/05/09      Ģɼ�ͼ����ѹ���ȼ������
      * 01.00.03          ���      18/12/04      �������򥳥��Ⱥ��
      *                                           ��׷����4�������
      ********************************************************************
       01  SC03.
      *����ǯ��
         03  SC03-PRTYM            PIC  X(16).
      *����ǯ����
         03  SC03-SEIYMD           PIC  X(22).
      *����
         03  SC03-ADRS             PIC  X(100).
      *���ŵ���̾
         03  SC03-HOSPNAME         PIC  X(100).
      *������̾
         03  SC03-KAISETUNAME      PIC  X(60).
      *���ŵ��إ�����
         03  SC03-HOSPCD           PIC  X(07).
      *����ɽ��ʬ
         03  SC3-TENSUKBN-TBL OCCURS 3.
             05  SC03-TENSUKBN     PIC X(02).
      *���ʻ�̾
         03  SC03-KANANAME-TBL  OCCURS 25.
             05  SC03-KANANAME     PIC  X(20).
      *��̾
         03  SC03-NAME-TBL   OCCURS  25.
             05  SC03-NAME         PIC  X(20).
      *��ǯ����
            03  SC03-BIRTHDAY-TBL    OCCURS 25 TIMES.
              05  SC03-BIRTHDAY-D      PIC X(2).
              05  SC03-BIRTHDAY-M      PIC X(2).
              05  SC03-BIRTHDAY-Y      PIC X(2).
      *�ݸ����̣�
         03  SC03-HKNKBN-TBL   OCCURS  25.
             05  SC03-HKNKBN-G OCCURS 6.
                 07  SC03-HKNKBN   PIC  X(02).
      *�ܿͲ�²��ʬ��
      *   03  SC03-HONKZKKBN1-TBL   OCCURS  20.
         03  SC03-HONKZKKBN1-TBL   OCCURS  25.
             05  SC03-HONKZKKBN    PIC  X(02).
      *������ô���ֹ�
         03  SC03-FTNJANUM1-TBL   OCCURS  25.
             05  SC03-FTNJANUM1    PIC  X(04).
      *������ô���ֹ�
         03  SC03-FTNJANUM2-TBL   OCCURS  25.
             05  SC03-FTNJANUM2    PIC  X(08).
      *������ֹ�
         03  SC03-JKYSNUM-TBL   OCCURS  25.
             05  SC03-JKYSNUM      PIC  X(14).
      *����ǯ��
         03  SC03-SRYY-TBL   OCCURS  25.
             05  SC03-SRYYM        PIC  X(08).
      *������ʬ��
         03  SC03-NYUGAIKBN1-TBL OCCURS  25.
             05  SC03-NYUGAIKBN1   PIC  X(02).
      *������ʬ��
         03  SC03-NYUGAIKBN2-TBL OCCURS  25.
             05  SC03-NYUGAIKBN2   PIC  X(02).
      *�ݸ�������
         03  SC03-TENSU-TBL     OCCURS  25.
             05  SC03-TENSU        PIC  X(09).
      *������ôʸ��
         03  SC03-YKZMOJI-TBL    OCCURS  25.
             05  SC03-YKZMOJI      PIC  X(02).
      *������ô��
         03  SC03-YKZMARU-TBL    OCCURS  25.
             05  SC03-YKZMARU      PIC  X(02).
      *������ô
         03  SC03-YKZFTN-TBL    OCCURS  25.
             05  SC03-YKZFTN       PIC  X(06).
      *��ˡ�����
         03  SC03-KSFTN-TBL    OCCURS  25.
             05  SC03-KSFTN        PIC  X(06).
      *����¾
         03  SC03-ETCFTN-TBL    OCCURS  25.
             05  SC03-ETCFTN       PIC  X(06).
      *������ô
         03  SC03-ITBFTN-TBL    OCCURS  25.
             05  SC03-ITBFTN       PIC  X(06).
      *����ʸ��
         03  SC03-SMOJI-TBL    OCCURS  25.
             05  SC03-SMOJI        PIC  X(02).
      *������
         03  SC03-SMARU-TBL    OCCURS  25.
             05  SC03-SMARU        PIC  X(02).
      *���ʸ��
         03  SC03-KMOJI-TBL    OCCURS  25.
             05  SC03-KMOJI        PIC  X(04).
      *��ˡ�
         03  SC03-KMARU-TBL    OCCURS  25.
             05  SC03-KMARU        PIC  X(02).
      *���
         03  SC03-GOKEI-TBL.
      *��׷��
             05  SC03-GKENSU       PIC  X(04).
      *�������
             05  SC03-GTENSU       PIC  X(09).
      *�������ʸ��
             05  SC03-GYKZMOJI     PIC  X(02).
      *������ޡ�
             05  SC03-GYKZMARU     PIC  X(02).
      *���������ô
             05  SC03-GYKZFTN      PIC  X(07).
      *��׷�ˡ�����
             05  SC03-GKSFTN       PIC  X(07).
      *��פ���¾
             05  SC03-GETCFTN      PIC  X(07).
      *��׼�����ô
             05  SC03-GITBFTN      PIC  X(07).
