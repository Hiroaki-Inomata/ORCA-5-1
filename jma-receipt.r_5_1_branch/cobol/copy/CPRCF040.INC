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
      *    ϫ�ҥ쥻�ץ����ٽ�             COPY (CPRCF040.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *
      *  03.03.01    NACL-ƣ��    06/09/19  ������Ǥλ�̾�ѹ��б�
      *                                     �쥻�ץ��о�ǯ���������ɲ�
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      *
      *  04.07.01    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
      *
      *  04.08.01    NACL-ƣ��    17/04/10  ����ñ�̤Ǥ�������������
      *                                     ��ϫ�ҽ�����Ϸ��ݽ����б�
      ************************************************************
      *01  RECE40-REC.
           03  RECE40-KEY.
      *���ŵ��أɣ�
               05  RECE40-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE40-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE40-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE40-PTID            PIC  9(10).
      *�쥻�ץȿ��Ų�
               05  RECE40-RECEKA          PIC  X(02).
      *���ݸ��ɣ�
               05  RECE40-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE40-TEKSTYMD        PIC  X(08). 
      *���ݸ��ݸ��ֹ�
           03  RECE40-HKNNUM              PIC  X(03).
      *�ݸ��ȹ礻�ֹ�
           03  RECE40-HKNCOMBI            PIC  9(04).
      *���Ųʾ���
           03 RECE40-SRYKAINF.
               05  RECE40-SRYKA           PIC  X(02)  OCCURS  20.
      *�軰�Թ԰ٻ��ΰ���ʬ���ݸ�����
           03  RECE40-HKN-X.
      *����ʬ���ݸ��ֹ�
               05  RECE40-HKNNUM-X        PIC  X(03).
      *����ʬ���ݸ��ɣ�
               05  RECE40-HKNID-X         PIC  9(10).
      *�����ն�ʬ
           03  RECE40-JIBAIKBN            PIC  X(01).
      *̤������ʬ
           03  RECE40-TEISEIKBN           PIC  X(01).
      *�쥻�ץ��о�ǯ��������
           03  RECE40-TEKYMD.
      *Ŭ�ѳ���ǯ����
               05  RECE40-TEKSTYMD-X      PIC  X(08). 
      *Ŭ�ѽ�λǯ����
               05  RECE40-TEKEDYMD-X      PIC  X(08). 
      
