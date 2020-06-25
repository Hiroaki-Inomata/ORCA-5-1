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
      *    ���ܥ쥻�ץȥ����ȥޥ���    COPY (CPRYOUYOCOM.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  RYOUYOCOM-REC.
           03  RYOCOM-KEY.
      *���ŵ��أɣ�
               05  RYOCOM-HOSPNUM         PIC 9(2).
      *������ʬ
               05  RYOCOM-NYUGAIKBN      PIC  X(01).
      *���ԣɣ�
               05  RYOCOM-PTID           PIC  9(10).
      *����ǯ��
               05  RYOCOM-SRYYM          PIC  X(06).
      *���ֹ�
               05  RYOCOM-ZAINUM         PIC  9(08).
      *�޼��̶�ʬ�ʣ������ģ��������������Ŷ�ʬ��
           03  RYOCOM-ZAISKBKBN          PIC  X(01).
      *���ť����ɡʥ����ȥ����ɡ�
           03  RYOCOM-SRYCD              PIC  X(09).
      *�������
           03  RYOCOM-MONTH              PIC  9(02).
      *��ʬ�������˥ơ��֥�
           03  RYOCOM-DAY-AREA.
      *        ��ʬ�����ơ��֥�
             04  RYOCOM-DAY-TBL.
      *        ���Ų���ʣ������飳������
               05  RYOCOM-DAY-1          PIC  9(02).
               05  RYOCOM-DAY-2          PIC  9(02).
               05  RYOCOM-DAY-3          PIC  9(02).
               05  RYOCOM-DAY-4          PIC  9(02).
               05  RYOCOM-DAY-5          PIC  9(02).
               05  RYOCOM-DAY-6          PIC  9(02).
               05  RYOCOM-DAY-7          PIC  9(02).
               05  RYOCOM-DAY-8          PIC  9(02).
               05  RYOCOM-DAY-9          PIC  9(02).
               05  RYOCOM-DAY-10         PIC  9(02).
               05  RYOCOM-DAY-11         PIC  9(02).
               05  RYOCOM-DAY-12         PIC  9(02).
               05  RYOCOM-DAY-13         PIC  9(02).
               05  RYOCOM-DAY-14         PIC  9(02).
               05  RYOCOM-DAY-15         PIC  9(02).
               05  RYOCOM-DAY-16         PIC  9(02).
               05  RYOCOM-DAY-17         PIC  9(02).
               05  RYOCOM-DAY-18         PIC  9(02).
               05  RYOCOM-DAY-19         PIC  9(02).
               05  RYOCOM-DAY-20         PIC  9(02).
               05  RYOCOM-DAY-21         PIC  9(02).
               05  RYOCOM-DAY-22         PIC  9(02).
               05  RYOCOM-DAY-23         PIC  9(02).
               05  RYOCOM-DAY-24         PIC  9(02).
               05  RYOCOM-DAY-25         PIC  9(02).
               05  RYOCOM-DAY-26         PIC  9(02).
               05  RYOCOM-DAY-27         PIC  9(02).
               05  RYOCOM-DAY-28         PIC  9(02).
               05  RYOCOM-DAY-29         PIC  9(02).
               05  RYOCOM-DAY-30         PIC  9(02).
               05  RYOCOM-DAY-31         PIC  9(02).
           03  RYOCOM-DAY-AREA-R         REDEFINES   RYOCOM-DAY-AREA.
      *        ��ʬ����ơ��֥�
               05  RYOCOM-DAY-TBL2.
                   07  RYOCOM-DAY        PIC  9(02)
                                         OCCURS   31.
      *ü���ɣ�
           03  RYOCOM-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  RYOCOM-OPID               PIC  X(16).
      *����ǯ����
           03  RYOCOM-CREYMD             PIC  X(08).
      *����ǯ����
           03  RYOCOM-UPYMD              PIC  X(08).
      *��������
           03  RYOCOM-UPHMS              PIC  X(06).
      *