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
      *    ������Ź԰٥ޥ����ղ�  COPY (CPWKSRYACTPLUS.INC)
      *
      *-------------------------------------------------*
      *03  WKSRYACTPLUS-REC.
      *������ʬ�ʣ��������
           05  WKSRYP-ADDKBN             PIC  X(01).
      *������ʬ�ʣ����ץ饹�������ޥ��ʥ���
           05  WKSRYP-PLUSKBN            PIC  9(01).
      *ϫ�Ҥα�
           05  WKSRYP-RSIKINGAKU         PIC  9(07).
      *�굻������ơʿǻ�����ѡ��������ǡ����֡�
           05  WKSRYP-SYUGI-TBL-G.
               07  WKSRYP-SYUGI-TBL      OCCURS   5.
      *���ϼ굻������
                   09  WKSRYP-SRYCD          PIC  X(09).
      *�굻����
                   09  WKSRYP-SYUTEN         PIC  9(07).
      *��������
                   09  WKSRYP-YKZTEN         PIC  9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                   09  WKSRYP-KIZAITEN       PIC  9(07).
      *�ե��������
                   09  WKSRYP-FIRTEN         PIC  9(07).
      *��������
           05  WKSRYP-SANSOTEN       PIC  9(07).
      *��������
           05  WKSRYP-CHISOTEN       PIC  9(07).
      *������������
           05  WKSRYP-INGAITEN       PIC  9(07).
      *
      *��ʬ����ٻ��
      *�����ե饰
           05  WKSRYP-MSANTEITFLG       PIC  9(01).
      *���������
           05  WKSRYP-MSANTEITEN        PIC  9(08).
      *�����������ʬ
           05  WKSRYP-MSANTEITENKBN     PIC  9(01).
      *
      *���������������оݡʣ����������ٺޡ�����������
           05  WKSRYP-TEI-NKBN           PIC  9(01).
      *H27.4
      *���������������оݡʣ����������ٺޡ�����������
           05  WKSRYP-TEI-NKBN30         PIC  9(01).
      *
      *API��
      *ϫ������
           05  WKSRYP-RSI-TENSU          PIC  9(07).
      *
