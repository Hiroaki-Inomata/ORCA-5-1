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
      *    �ģУô��Ի�����ܾ���        COPY (CPDPCPTKHN.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  DPCPTKHN-REC.
      *����
           03  DPCPTKHN-KEY.
      *���ŵ��أɣ�
               05  DPCPTKHN-HOSPNUM            PIC  9(2).
      *���ԣɣ�
               05  DPCPTKHN-PTID               PIC  9(10).
      *�ģУ������ֹ�ʣ�������κ��������θ���������ֹ��
               05  DPCPTKHN-DPCRRKNUM          PIC  9(03).
      *�ģУ������ֹ����
               05  DPCPTKHN-DPCRRKEDANUM       PIC  9(03).
      *�ģУ������ֹ�������
           03  DPCPTKHN-DPCMAXEDANUM           PIC  9(03).
      *�ģУû�������������ʬ��0:�������ʤ�/1:���������
           03  DPCPTKHN-DPCTEKKBN              PIC  X(01).
      *Ŭ�ѳ�����
           03  DPCPTKHN-TEKSTYMD               PIC  X(08).
      *Ŭ�ѳ�������ʬ��1:������/2:ž������
           03  DPCPTKHN-TEKSTKBN               PIC  X(01).
      *Ŭ�ѽ�λ��
           03  DPCPTKHN-TEKEDYMD               PIC  X(08).
      *Ŭ�ѽ�λ����ʬ��1:�ౡ��/2:ž������
           03  DPCPTKHN-TEKEDKBN               PIC  X(01).
      *Ʊ����¤ǤΣ������������ͽ���̵ͭ��0:�ʤ�/1:�����
           03  DPCPTKHN-SAINYUKBN              PIC  X(01).
      *���Ƿ�ʬ���ֹ�
           03  DPCPTKHN-DPC-CD                 PIC  X(14).
      *�ģУ�ž����ʬ
           03  DPCPTKHN-DPCTENKIKBN            PIC  X(01).
      *���
           03  DPCPTKHN-COD                    PIC  X(100).
      *��ʬ�ֹ�
           03  DPCPTKHN-KBNNUM                 PIC  X(10).
      *�»ܡ�ͽ���ǯ����
           03  DPCPTKHN-JISSIYMD               PIC  X(08).
      *���Ŷ�ʬ������
           03  DPCPTKHN-SRYKBNCD               PIC  X(04).
      *����̾��
           03  DPCPTKHN-STYNAME                PIC  X(200).
      *ü���ɣ�
           03  DPCPTKHN-TERMID                 PIC  X(16).
      *���ڡ��졼���ɣ�
           03  DPCPTKHN-OPID                   PIC  X(16).
      *����ǯ����
           03  DPCPTKHN-CREYMD                 PIC  X(08).
      *����ǯ����
           03  DPCPTKHN-UPYMD                  PIC  X(08).
      *��������
           03  DPCPTKHN-UPHMS                  PIC  X(06).
      *�ʲ�virtual����
           03  DPCPTKHN-RRKNUM                 PIC  9(03).
      *�����ȥե饰
           03  DPCPTKHN-SORTFLG                PIC  9(01).