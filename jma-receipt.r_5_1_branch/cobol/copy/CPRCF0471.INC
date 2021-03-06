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
      *    �쥻�ץ����ٽ�     COPY (CPRCF047.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *  01.00.01    NACL-����    05.07.28  ���ֹ��ɲ�
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      ************************************************************
      *01  RECE471-REC.
           03  RECE471-KEY.
      *���ŵ��أɣ�
               05  RECE471-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE471-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE471-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE471-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE471-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE471-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE471-TEKSTYMD        PIC  X(08). 
      *���Ŷ�ʬ
               05  RECE471-SRYKBN          PIC  X(02).
      *
               05  RECE471-GAITENTTLKBN    PIC  9(03).
      *���ť����ɡʺޤ���Ƭ�ο��ť����ɡ�
               05  RECE471-SRYCD1          PIC  X(09).                    
      *���ֹ�
               05  RECE471-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE471-LINENUM         PIC  9(05).
      *�����ֹ�
           03  RECE471-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE471-SRYKA               PIC  X(02).
      *Ŭ�������
           03  RECE471-TEKIYO-INF.
      *�޶��ڡʡ���
               05  RECE471-ASTER           PIC  X(02).
      *Ŧ��
               05  RECE471-TEKIYO          PIC  X(100).
