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
      *    �쥻�ץ����ٽ�������� ��ñ�ѹ��ܥե�����             *
      *                               COPY (CPRCF067.INC)        *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  00.00.01    NACL-ƣ��    04/04/07  ��������ʾ������ɲ�
      *  01.00.02    NACL-ƣ��    04/08/23  �õ�����ʥ쥻���ѡˤ��ɲ�
      *
      *  02.06.01    NACL-ƣ��    05/09/12  ������ô��ʥ쥻���ѡ��ɲ�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      ************************************************************
      *01  RECE67-REC.
           03  RECE67-KEY.
      *���ŵ��أɣ�
               05  RECE67-HOSPNUM         PIC  9(2).
      *����ǯ��
               05  RECE67-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE67-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE67-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE67-RECEKA          PIC  X(02).
      *������ʧ����
               05  RECE67-TEISYUTUSAKI    PIC  9(01).
      *�쥻�ץȼ���
               05  RECE67-RECESYUBETU     PIC  9(04).
      *�ݸ����ֹ�        
               05  RECE67-HKNJANUM        PIC  X(08).
      *�����ʬ
               05  RECE67-HOJOKBN         PIC  X(01).
      *����ɣ�
               05  RECE67-KOHID           PIC 9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE67-TEKSTYMD        PIC  X(08). 
      *ʬ���ʬ
               05  RECE67-RECEIPT-KBN     PIC  X(10).
      *���ݸ��ݵ��ճ��
           03  RECE67-KYURATE             PIC  9(03).
      *���ܤε��վ���
           03  RECE67-TOTALTENINF         OCCURS  5.
      *�������
               05  RECE67-TOTALTEN        PIC  9(08).
      *��ô���
               05  RECE67-FTNMONEY        PIC  9(09).
      *������
               05  RECE67-JNISSU          PIC  9(02).
      *�������������
           03  RECE67-SHOKUJIINF          OCCURS  5.
      *��������
               05  RECE67-SHOKUJINISSU    PIC  9(02).
      *����������
               05  RECE67-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  RECE67-SHOKUJIFTN      PIC  9(09).
      *�������
           03  RECE67-KOHINF              OCCURS  4.
      *�������ô���ֹ�
               05  RECE67-KOHFTNJANUM     PIC  X(08).
      *����ݼ�����ֹ�
               05  RECE67-KOHJKYSNUM      PIC  X(20).
      *�õ��������
           03  RECE67-TOKKIINF            OCCURS  5.
               05  RECE67-TOKKI           PIC  X(02).
      *���ܤε��վ���ʥ쥻���ѡ�
           03  RECE67-RECEDEN-TOTALTENINF OCCURS  5.
      *��ô���
               05  RECE67-RECEDEN-FTNMONEY
                                          PIC  9(09).
      *��������ʾ�����
           03  RECE67-HEAD4-G.
               05  RECE67-HEAD4           PIC  X(62)   OCCURS  2.
      *Ģɼ�ǡ���
           03  RECE67-PRTDATA             PIC  X(4500).
