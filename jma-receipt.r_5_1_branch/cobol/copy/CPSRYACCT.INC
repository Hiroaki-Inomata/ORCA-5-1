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
      *    ���Ų�ץޥ���    COPY (CPSRYACCT.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  13/01/11  ���Ų���ơ��֥룱���б�
      ************************************************************
      *01  SRYACCT-REC.
           03  ACCT-KEY.
      *���ŵ��أɣ�
               05  ACCT-HOSPNUM          PIC 9(2).
      *������ʬ
               05  ACCT-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  ACCT-PTID            PIC  9(10).
      *���Ų�
               05  ACCT-SRYKA           PIC  X(02).
      *����ǯ��
               05  ACCT-SRYYM           PIC  X(06).
      *���Ŷ�ʬ
               05  ACCT-SRYKBN          PIC  X(02).
      *���ֹ�
               05  ACCT-ZAINUM          PIC  9(08).
      *�ݸ��ȹ礻
           03  ACCT-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ�����޶�ʬ��
           03  ACCT-JIHOKBN             PIC  X(01).
      *������
           03  ACCT-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  ACCT-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  ACCT-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ٿ�
           03  ACCT-MEISAISU            PIC  9(07).
      *�޲��
           03  ACCT-ZAIKAISU            PIC  9(07).
      *�굻������
           03  ACCT-SYUTEN1             PIC  9(07).
      *�굻������
           03  ACCT-SYUTEN2             PIC  9(07).
      *��������
           03  ACCT-YKZTEN              PIC  9(07).
      *�������
           03  ACCT-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  ACCT-DRCD                PIC  X(05).
      *������ե饰���ղå쥳���ɥե饰��
           03  ACCT-ZAIREQFLG           PIC  X(01).
      *����Ψ
           03  ACCT-TEIGENRATE          PIC  9(03).
      *�޼��̶�ʬ�ʣ�����ɾ����������ʬ��
           03  ACCT-ZAIKBN              REDEFINES
               ACCT-TEIGENRATE          PIC  9(03).
      *���Ų���ơ��֥�
           03  ACCT-DAY-AREA.
      ******** ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
      ****** 04  ACCT-DAY-TBL         OCCURS  4.
      *        ���Ų���ơ��֥��1:���,2��10������������
             04  ACCT-DAY-TBL         OCCURS  10.
      *        ���Ų���ʣ������飳������
               05  ACCT-DAY-1           PIC  9(03).
               05  ACCT-DAY-2           PIC  9(03).
               05  ACCT-DAY-3           PIC  9(03).
               05  ACCT-DAY-4           PIC  9(03).
               05  ACCT-DAY-5           PIC  9(03).
               05  ACCT-DAY-6           PIC  9(03).
               05  ACCT-DAY-7           PIC  9(03).
               05  ACCT-DAY-8           PIC  9(03).
               05  ACCT-DAY-9           PIC  9(03).
               05  ACCT-DAY-10          PIC  9(03).
               05  ACCT-DAY-11          PIC  9(03).
               05  ACCT-DAY-12          PIC  9(03).
               05  ACCT-DAY-13          PIC  9(03).
               05  ACCT-DAY-14          PIC  9(03).
               05  ACCT-DAY-15          PIC  9(03).
               05  ACCT-DAY-16          PIC  9(03).
               05  ACCT-DAY-17          PIC  9(03).
               05  ACCT-DAY-18          PIC  9(03).
               05  ACCT-DAY-19          PIC  9(03).
               05  ACCT-DAY-20          PIC  9(03).
               05  ACCT-DAY-21          PIC  9(03).
               05  ACCT-DAY-22          PIC  9(03).
               05  ACCT-DAY-23          PIC  9(03).
               05  ACCT-DAY-24          PIC  9(03).
               05  ACCT-DAY-25          PIC  9(03).
               05  ACCT-DAY-26          PIC  9(03).
               05  ACCT-DAY-27          PIC  9(03).
               05  ACCT-DAY-28          PIC  9(03).
               05  ACCT-DAY-29          PIC  9(03).
               05  ACCT-DAY-30          PIC  9(03).
               05  ACCT-DAY-31          PIC  9(03).
           03  ACCT-DAY-AREA-R           REDEFINES   ACCT-DAY-AREA.
      ******   ���Ų���ơ��֥��1:���,2:����,3:����, 4:����5:����)
      *******  05  ACCT-DAY-TBL2        OCCURS  4.
      *        ���Ų���ơ��֥��1:���,2��10������������
               05  ACCT-DAY-TBL2        OCCURS  10.
                   07  ACCT-DAY         PIC  9(03)
                                        OCCURS   31.
      *ü���ɣ�
           03  ACCT-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  ACCT-OPID                PIC  X(16).
      *����ǯ����
           03  ACCT-CREYMD              PIC  X(08).
      *����ǯ����
           03  ACCT-UPYMD               PIC  X(08).
      *��������
           03  ACCT-UPHMS               PIC  X(06).
      *����ǯ����ѹ���
           03  ACCT-STSRYYM             PIC  X(06).
           03  ACCT-EDSRYYM             PIC  X(06).
