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
      *    �����������     COPY (CPSKT0600TMP.INC)            *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����      ����
      *  01.00.01    ����    08/10/08  �����ɡ�2007�׸���ޤȤ��б�
      *  01.00.02    ����    12/04/17  ����ʬ����ƻ�ܸ����Ȥ˽��פ���
      ************************************************************
       01  SKT1100TMP-REC.
           03      SKT1100TMP-KEY.
             05    SKT1100TMP-KENKBN                PIC  9(01).
             05    SKT1100TMP-KBN1                  PIC  9(01).
             05    SKT1100TMP-KBN2                  PIC  9(01).
             05    SKT1100TMP-HKNJANUM              PIC  X(06).
             05    SKT1100TMP-KYURATE               PIC  9(02).
      *=== �����ɡ�2007���б� by Yoshikawa(2008/10/08) start
           03      SKT1100TMP-KOHNAME               PIC  X(10).
      *=== �����ɡ�2007���б� by Yoshikawa(2008/10/08) end
      *=== ��������ƻ�ܸ����Ȥ˽��� by Yoshikawa(2012/04/17) start
           03      SKT1100TMP-HKNJANAME             PIC  X(20).
      *=== ��������ƻ�ܸ����Ȥ˽��� by Yoshikawa(2012/04/17) end
           03      SKT1100TMP-DATA.
             05    SKT1100TMP-GAIRAI.
               07  SKT1100TMP-KENSU               PIC  9(05).
               07  SKT1100TMP-NISSU               PIC  9(05).
               07  SKT1100TMP-TENSU               PIC  9(09).
               07  SKT1100TMP-SEIKYUGK            PIC  9(09).