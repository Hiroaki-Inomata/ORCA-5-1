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
      *    �ģУô��Ծ���        COPY (CPDPCPTINF.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  DPCPTINF-REC.
      *����
           03  DPCPTINF-KEY.
      *���ŵ��أɣ�
               05  DPCPTINF-HOSPNUM            PIC  9(2).
      *���ԣɣ�
               05  DPCPTINF-PTID               PIC  9(10).
      *�ģУ������ֹ�
               05  DPCPTINF-DPCRRKNUM          PIC  9(03).
      *�ģУ�������
               05  DPCPTINF-DPCSTYMD           PIC  X(08).
      *�ģУ��ౡ��
               05  DPCPTINF-DPCEDYMD           PIC  X(08).
      *�ģУõ�����
               05  DPCPTINF-BASEYMD            PIC  X(08).
      *��������ʳ��������ư̵ͭ��0:�ʤ�/1:�����
           03  DPCPTINF-IDOKBN                 PIC  X(01).
      *ͽ�ꡦ�۵�������ʬ
           03  DPCPTINF-YOTEIKBN               PIC  X(01).
      *�����ౡǯ����
           03  DPCPTINF-ZENTAIINYMD            PIC  X(08).
      *����Ʊ����¤Ǥ�������̵ͭ��0:�ʤ�/1:�����
           03  DPCPTINF-DOBYOKBN               PIC  X(01).
      *�������ν�
           03  DPCPTINF-BIRTHWT                PIC  X(04).
      *�ʣã�
           03  DPCPTINF-JCS                    PIC  X(03).
      *Burn Index
           03  DPCPTINF-BI                     PIC  9(03)V9(01).
      *�ž�����
           03  DPCPTINF-JUSHODO                PIC  X(100).
      *ü���ɣ�
           03  DPCPTINF-TERMID                 PIC  X(16).
      *���ڡ��졼���ɣ�
           03  DPCPTINF-OPID                   PIC  X(16).
      *����ǯ����
           03  DPCPTINF-CREYMD                 PIC  X(08).
      *����ǯ����
           03  DPCPTINF-UPYMD                  PIC  X(08).
      *��������
           03  DPCPTINF-UPHMS                  PIC  X(06).
      *�ʲ�virtual����
      *���������ֹ�
           03  DPCPTINF-RRKNUM                 PIC  9(03).