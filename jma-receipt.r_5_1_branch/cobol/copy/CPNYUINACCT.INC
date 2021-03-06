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
      *    ������ץޥ���    COPY (CPNYUINACCT.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  NYUIN-SRYACCT-REC.
           03  NACCT-KEY.
      *���ŵ��أɣ�
               05  NACCT-HOSPNUM          PIC 9(2).
      *������ʬ
               05  NACCT-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  NACCT-PTID            PIC  9(10).
      *���Ų�
               05  NACCT-SRYKA           PIC  X(02).
      *����ǯ��
               05  NACCT-SRYYM           PIC  X(06).
      *���Ŷ�ʬ
               05  NACCT-SRYKBN          PIC  X(02).
      *���ֹ�
               05  NACCT-ZAINUM          PIC  9(08).
      *�޼��̶�ʬ�ʣ������������������񡡣����������ۡ���������
      *            �����ݸ��ȹ礻�������������û�����������ô��������
           03  NACCT-ZAISKBKBN           PIC  X(01).           
      *�ݸ��ȹ礻
           03  NACCT-HKNCOMBI            PIC  9(04).
      *������
           03  NACCT-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  NACCT-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  NACCT-SURYOUTOTAL         PIC  9(07)V9(03).
      *���ٿ�
           03  NACCT-MEISAISU            PIC  9(07).
      *�޲��
           03  NACCT-ZAIKAISU            PIC  9(07).
      *�굻������
           03  NACCT-SYUTEN1             PIC  9(07).
      *�굻������
           03  NACCT-SYUTEN2             PIC  9(07).
      *��������
           03  NACCT-YKZTEN              PIC  9(07).
      *�������
           03  NACCT-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  NACCT-DRCD                PIC  X(05).
      *���Ų���ơ��֥�
           03  NACCT-DAY-AREA.
      *        ���Ų���ơ��֥�
             04  NACCT-DAY-TBL.
      *        ���Ų���ʣ������飳������
               05  NACCT-DAY-1           PIC  9(03).
               05  NACCT-DAY-2           PIC  9(03).
               05  NACCT-DAY-3           PIC  9(03).
               05  NACCT-DAY-4           PIC  9(03).
               05  NACCT-DAY-5           PIC  9(03).
               05  NACCT-DAY-6           PIC  9(03).
               05  NACCT-DAY-7           PIC  9(03).
               05  NACCT-DAY-8           PIC  9(03).
               05  NACCT-DAY-9           PIC  9(03).
               05  NACCT-DAY-10          PIC  9(03).
               05  NACCT-DAY-11          PIC  9(03).
               05  NACCT-DAY-12          PIC  9(03).
               05  NACCT-DAY-13          PIC  9(03).
               05  NACCT-DAY-14          PIC  9(03).
               05  NACCT-DAY-15          PIC  9(03).
               05  NACCT-DAY-16          PIC  9(03).
               05  NACCT-DAY-17          PIC  9(03).
               05  NACCT-DAY-18          PIC  9(03).
               05  NACCT-DAY-19          PIC  9(03).
               05  NACCT-DAY-20          PIC  9(03).
               05  NACCT-DAY-21          PIC  9(03).
               05  NACCT-DAY-22          PIC  9(03).
               05  NACCT-DAY-23          PIC  9(03).
               05  NACCT-DAY-24          PIC  9(03).
               05  NACCT-DAY-25          PIC  9(03).
               05  NACCT-DAY-26          PIC  9(03).
               05  NACCT-DAY-27          PIC  9(03).
               05  NACCT-DAY-28          PIC  9(03).
               05  NACCT-DAY-29          PIC  9(03).
               05  NACCT-DAY-30          PIC  9(03).
               05  NACCT-DAY-31          PIC  9(03).
           03  NACCT-DAY-AREA-R          REDEFINES   NACCT-DAY-AREA.
      *        ���Ų���ơ��֥�
               05  NACCT-DAY-TBL2.
                   07  NACCT-DAY         PIC  9(03)
                                         OCCURS   31.
      *ü���ɣ�
           03  NACCT-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  NACCT-OPID                PIC  X(16).
      *����ǯ����
           03  NACCT-CREYMD              PIC  X(08).
      *����ǯ����
           03  NACCT-UPYMD               PIC  X(08).
      *��������
           03  NACCT-UPHMS               PIC  X(06).
      *
