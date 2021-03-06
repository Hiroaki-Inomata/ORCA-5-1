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
      *-------------------------------------------------*
      *    ���Ų�ץޥ����ղ�  COPY (CPSRYACCTPLUS.INC)
      *
      *-------------------------------------------------*
      *01  SRYACCTPLUS-REC.
           03  ACCTP-KEY.
      *���ŵ��أɣ�
               05  ACCTP-HOSPNUM         PIC 9(2).
      *������ʬ
               05  ACCTP-NYUGAIKBN      PIC  X(01).
      *���ԣɣ�
               05  ACCTP-PTID           PIC  9(10).
      *���Ų�
               05  ACCTP-SRYKA          PIC  X(02).
      *����ǯ��
               05  ACCTP-SRYYM          PIC  X(06).
      *���ֹ�
               05  ACCTP-ZAINUM         PIC  9(08).
      *Ϣ�ֹ���̾�ϣ���Τߡ��������������ξ��ʣ����
               05  ACCTP-RENNUM         PIC  9(02).
      *������ʬ�ʣ����ץ饹�������ޥ��ʥ���
           03  ACCTP-PLUSKBN            PIC  9(01).
      *ϫ�Ҥα�
           03  ACCTP-RSIKINGAKU         PIC  9(07).
      *�굻������ơʼ�ѡ��������ǡ����֡�
           03  ACCTP-SYUGI-TBL-G.
               05  ACCTP-SYUGI-TBL      OCCURS   5.
      *�굻���ϥ�����
                   07  ACCTP-SRYCD          PIC  X(09).
      *�굻����
                   07  ACCTP-SYUTEN         PIC  9(07).
      *��������
                   07  ACCTP-YKZTEN         PIC  9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                   07  ACCTP-KIZAITEN       PIC  9(07).
      *�ե��������
                   07  ACCTP-FIRTEN         PIC  9(07).
      *��������
           03  ACCTP-SANSOTEN       PIC  9(07).
      *��������
           03  ACCTP-CHISOTEN       PIC  9(07).
      *��������������
           03  ACCTP-INGAITEN          PIC  9(07).
      *
      *���������������оݺ��ֹ�
           03  ACCTP-GRP-ZAINUM-G.
               05  ACCTP-GRP-ZAINUM        PIC 9(08)
                                        OCCURS   15.
      *ü���ɣ�
           03  ACCTP-TERMID            PIC  X(16).
      *���ڡ��졼���ɣ�
           03  ACCTP-OPID              PIC  X(16).
      *����ǯ����
           03  ACCTP-CREYMD            PIC  X(08).
      *����ǯ����
           03  ACCTP-UPYMD             PIC  X(08).
      *��������
           03  ACCTP-UPHMS             PIC  X(06).
      *
