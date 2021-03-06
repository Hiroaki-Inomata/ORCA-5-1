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
      *    �����쥻�ץȡ������ˡ���������   COPY (CPRCF221.INC) 
      *                       ���Ųʰ㤤��Ʊ��ޤΤޤȤ���         
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.07.01    NACL-����    13/01/22  �ǡ����ΰ��ĥ
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-����    16/08/23  �������б�
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE221-REC.
           03  RECE221-KEY.
               05  RECE221-KEY1.
      *���ŵ����ֹ�
                   07  RECE221-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE221-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE221-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE221-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE221-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE221-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE221-TEKSTYMD    PIC X(08). 
      *���Ŷ�ʬ
               05  RECE221-SRYKBN          PIC X(02).
      *���ż��̶�ʬ
               05  RECE221-SRYSYUKBN       PIC X(03).
      *������
               05  RECE221-ZAITEN          PIC 9(08).
      *���ť����ɷ�
               05  RECE221-SRYCDTOTAL      PIC 9(14).
      *���̷�
               05  RECE221-SURYOUTOTAL     PIC 9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE221-RUIKEITOTAL     PIC 9(13)V9(05).
      *���ٿ�
               05  RECE221-MEISAISU        PIC 9(04).
      *���ݶ�ʬ
               05  RECE221-JIHOKBN         PIC X(01).
      *��������ʬ
               05  RECE221-JISANYAKKBN     PIC X(01).
      *�����Ⱥ�Ϣ��
               05  RECE221-COMRENNUM       PIC 9(05).
      *�޲��
           03  RECE221-ZAIKAISU            PIC 9(04).
      *��������ơ��֥�
           03  RECE221-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
               05  RECE221-DAY-TBL         OCCURS  4.
                   07  RECE221-KAISUINF    OCCURS  31.
                       09  RECE221-KAISU   PIC 9(03).
      *�ǡ���
           03  RECE221-REC1                PIC X(1000).
      *�����ȥǡ���
           03  RECE221-COM-AREA.
               05  RECE221-COM             PIC X(300)  OCCURS  10.
      *��ǡ�Ʊ��ʣ�����Ųʡ˻����
           03  RECE221-SYODOU-SRYKA-AREA.
               05  RECE221-SYODOU-SRYKA    PIC X(02)   OCCURS  10.
