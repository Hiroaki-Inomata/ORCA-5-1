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
      *    ϫ�ҥ쥻�ץ����ٽ��������     COPY (CPRCF0842.INC)   *
      *                       ���Ųʰ㤤��Ʊ��ޤΤޤȤ���       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    04.02.05  ���ť����ɡ����̤��߷��ɲ�
      *  01.00.02    NACL-����    04.08.17  �쥻�ץȲ��ɲ�
      *  01.00.03    NACL-����    05.07.05  ���ż��̶�ʬ�ɲ�
      *  01.00.04    NACL-����    05.11.11  �����ȥǡ����ɲ�
      *  02.09.01    NACL-����    05.04.17  ��������ǯ��������б�
      *  03.05.01    NACL-����    07.06.01  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *
      *  04.07.01    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-����    16/08/23  �������б�
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE842-REC.
           03  RECE842-KEY.
      *���ŵ��أɣ�
               05  RECE842-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE842-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE842-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE842-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE842-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE842-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE842-TEKSTYMD        PIC  X(08).
      *���Ŷ�ʬ
               05  RECE842-SRYKBN          PIC  X(02).
      *���ż��̶�ʬ
               05  RECE842-SRYSYUKBN       PIC  X(03).
      *������
               05  RECE842-ZAITEN          PIC  9(08).
      *���ť����ɷ�
               05  RECE842-SRYCDTOTAL      PIC  9(14).
      *���̷�
               05  RECE842-SURYOUTOTAL     PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE842-RUIKEITOTAL     PIC  9(13)V9(05).
      *���ٿ�
               05  RECE842-MEISAISU        PIC  9(04).
      *���ݶ�ʬ
               05  RECE842-JIHOKBN         PIC  X(01).
      *��������ʬ
               05  RECE842-JISANYAKKBN     PIC  X(01).
      *�����Ⱥ�Ϣ��
               05  RECE842-COMRENNUM       PIC  9(05).
      *�޲��
           03  RECE842-ZAIKAISU            PIC  9(04).
      *��������ơ��֥�
           03  RECE842-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
               05  RECE842-DAY-TBL         OCCURS  4.
                   07  RECE842-KAISUINF    OCCURS  31.
                       09  RECE842-KAISU   PIC  9(03).
      *�ǡ���
           03  RECE842-REC1                PIC  X(1000).
      *�����ȥǡ���
           03  RECE842-COM-AREA.
               05  RECE842-COM             PIC  X(300)    OCCURS  10.
      *��ǡ�Ʊ��ʣ�����Ųʡ˻����
           03  RECE842-SYODOU-SRYKA-AREA.
               05  RECE842-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
