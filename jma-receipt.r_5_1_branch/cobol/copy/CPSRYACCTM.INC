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
      *    �����Ų�ץޥ����ʼ��     COPY (CPSRYACCTM.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SRYACCTM-REC.
           03  ACCTM-KEY.
      *���ŵ��أɣ�
               05  ACCTM-HOSPNUM          PIC 9(2).
      *������ʬ
               05  ACCTM-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  ACCTM-PTID            PIC  9(10).
      *���Ų�
               05  ACCTM-SRYKA           PIC  X(02).
      *����ǯ��
               05  ACCTM-SRYYM           PIC  X(06).
      *���Ŷ�ʬ
               05  ACCTM-SRYKBN          PIC  X(02).
      *���ֹ�
               05  ACCTM-ZAINUM          PIC  9(08).
      *�ݸ��ȹ礻
           03  ACCTM-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
           03  ACCTM-JIHOKBN             PIC  X(01).
      *������
           03  ACCTM-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  ACCTM-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  ACCTM-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ٿ�
           03  ACCTM-MEISAISU            PIC  9(07).
      *�޲��
           03  ACCTM-ZAIKAISU            PIC  9(07).
      *�굻������
           03  ACCTM-SYUTEN1             PIC  9(07).
      *�굻������
           03  ACCTM-SYUTEN2             PIC  9(07).
      *��������
           03  ACCTM-YKZTEN              PIC  9(07).
      *�������
           03  ACCTM-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  ACCTM-DRCD                PIC  X(05).
      *������ե饰
           03  ACCTM-ZAIREQFLG           PIC  X(01).
      *����Ψ
           03  ACCTM-TEIGENRATE          PIC  9(03).
      *����Ϣ��
           03  ACCTM-RENNUMMAX           PIC  9(03).
      *���Ų���ơ��֥�
           03  ACCTM-DAY-AREA.
      *        ���Ų���ơ��֥�ʹ�ס��������飳������
               05  ACCTM-DAY-1           PIC  9(03).
               05  ACCTM-DAY-2           PIC  9(03).
               05  ACCTM-DAY-3           PIC  9(03).
               05  ACCTM-DAY-4           PIC  9(03).
               05  ACCTM-DAY-5           PIC  9(03).
               05  ACCTM-DAY-6           PIC  9(03).
               05  ACCTM-DAY-7           PIC  9(03).
               05  ACCTM-DAY-8           PIC  9(03).
               05  ACCTM-DAY-9           PIC  9(03).
               05  ACCTM-DAY-10          PIC  9(03).
               05  ACCTM-DAY-11          PIC  9(03).
               05  ACCTM-DAY-12          PIC  9(03).
               05  ACCTM-DAY-13          PIC  9(03).
               05  ACCTM-DAY-14          PIC  9(03).
               05  ACCTM-DAY-15          PIC  9(03).
               05  ACCTM-DAY-16          PIC  9(03).
               05  ACCTM-DAY-17          PIC  9(03).
               05  ACCTM-DAY-18          PIC  9(03).
               05  ACCTM-DAY-19          PIC  9(03).
               05  ACCTM-DAY-20          PIC  9(03).
               05  ACCTM-DAY-21          PIC  9(03).
               05  ACCTM-DAY-22          PIC  9(03).
               05  ACCTM-DAY-23          PIC  9(03).
               05  ACCTM-DAY-24          PIC  9(03).
               05  ACCTM-DAY-25          PIC  9(03).
               05  ACCTM-DAY-26          PIC  9(03).
               05  ACCTM-DAY-27          PIC  9(03).
               05  ACCTM-DAY-28          PIC  9(03).
               05  ACCTM-DAY-29          PIC  9(03).
               05  ACCTM-DAY-30          PIC  9(03).
               05  ACCTM-DAY-31          PIC  9(03).
           03  ACCTM-DAY-AREA-R          REDEFINES   ACCTM-DAY-AREA.
               05  ACCTM-DAY             PIC  9(03)
                                         OCCURS   31.
      *ü���ɣ�
           03  ACCTM-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  ACCTM-OPID                PIC  X(16).
      *����ǯ����
           03  ACCTM-CREYMD              PIC  X(08).
      *����ǯ����
           03  ACCTM-UPYMD               PIC  X(08).
      *��������
           03  ACCTM-UPHMS               PIC  X(06).
      *����ǯ����ѹ���
           03  ACCTM-STSRYYM             PIC  X(06).
           03  ACCTM-EDSRYYM             PIC  X(06).
