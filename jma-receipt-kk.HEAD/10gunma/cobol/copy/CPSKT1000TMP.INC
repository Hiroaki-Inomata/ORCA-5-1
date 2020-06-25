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
      *    ����ɽ����ե�����                                    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/08/25  SKT1000TMP-TYOKI�ɲ�
      *  01.00.02    ����    2011/03/10  KBN����ͤ���ʸ�����ѹ�
      *  01.00.03    ����    2011/04/08  ����������ʿ�β��Ͽ��б�
      *  01.00.04    ����    2011/10/28  ��Į¼��ǲ��Ǥ���ʤ��褦�˽���
      *  01.00.05    ����    2011/11/04  ������ν���
      *  01.00.06    ����    2012/01/20  �ҳ��������硢�����ι��ݤ�
      *                                  �ȹ���ݤ���Ģɼ�ˤʤ�Τ���
      *  01.00.07    ���    2018/10/02  ���ץ����Ȥ���
      ************************************************************
       01  SKT1000TMP-REC.
           03  SKT1000TMP-KEY.
               05  SKT1000TMP-SRYYM          PIC  X(06).
               05  SKT1000TMP-PREFKBN        PIC  9(01).
               05  SKT1000TMP-TYOKI          PIC  X(01).
               05  SKT1000TMP-GENMEN         PIC  X(01).
               05  SKT1000TMP-ADRS           PIC  X(40).
               05  SKT1000TMP-KUMIAIKBN      PIC  X(01).
               05  SKT1000TMP-HKNJANUM       PIC  X(08).
               05  SKT1000TMP-ZAITAKU        PIC  X(01).
               05  SKT1000TMP-SYUBETU        PIC  9(01).
               05  SKT1000TMP-JYUNJO         PIC  9(02).
               05  SKT1000TMP-KBN            PIC  X(01).
               05  SKT1000TMP-KYURATE        PIC  9(02).
           03  SKT1000TMP-DATA.
               05  SKT1000TMP-KENSU          PIC  9(06).
               05  SKT1000TMP-KKENSU         PIC  9(06).
               05  SKT1000TMP-FKENSU         PIC  9(06).
               05  SKT1000TMP-TENSU          PIC  9(10).
               05  SKT1000TMP-NYUGAIKBN      PIC  9(01).
      *