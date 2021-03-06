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
       01  SC15.
      *����ǯ��
           03  SC15-PRTYM               PIC  X(16).
      *����ǯ����
           03  SC15-SEIYMD              PIC  X(22).
      *����ǯ����
           03  SC15-SHOMEIYMD           PIC  X(22).
      *��Į¼̾
           03  SC15-CITYNAME            PIC  X(22).
      *�����Ի�̾
           03  SC15-SEINAME             PIC  X(20).
      *���ŵ��ؽ���
           03  SC15-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SC15-HOSPNAME            PIC  X(100).
      *���߼Ի�̾
           03  SC15-KAISETU             PIC  X(40).
      *����Ի�ʾ��ֹ�
           03  SC15-JKYSNUM             PIC  X(14).
      *Ϸ���ݷ������ֹ�
           03  SC15-RJN-JKYSNUM         PIC  X(14).
      *����Խ���
           03  SC15-ADRS                PIC  X(80).
      *����Ի�̾
           03  SC15-NAME                PIC  X(30).
      *��ǯ����
           03  SC15-TANJOYMD            PIC  X(22).
      *�����ʬ
           03  SC15-KOHKBN              PIC  X(02) OCCURS 2.
      *����ơ��֥�
           03  SC15-SEIKYU-TBL.
      *�ݸ�����
               05  SC15-HKNSYUBETU OCCURS 6 PIC X(02).
      *�ݸ���ʬ(����)
               05  SC15-HKNKBN             PIC X(02).
      *�����ǡ���
               05  SC15-NYUIN-TBL.
      *��������������
                   07  SC15-N-IRYOGK       PIC X(18).
      *�����ݰ�����ô��(�ܿ���ô��)
                   07  SC15-N-ITBFTNGK     PIC X(18).
      *������ɸ����ô
                   07  SC15-N-HYJFTN       PIC X(18).
      *��������ǯ����
                   07  SC15-N-NYUINYMD     PIC X(22).
      *�����ݷ�³
                   07  SC15-KEIZOKU        PIC X(02).
      *��������������
                   07  SC15-N-NISSU        PIC X(04).
      *����ǡ���
               05  SC15-GAIRAI-TBL.
      *�������������
                   07  SC15-G-IRYOGK       PIC X(18).
      *����ݰ�����ô��(�ܿ���ô��)
                   07  SC15-G-ITBFTNGK     PIC X(18).
      *������̱�����
                   07  SC15-G-NISSU        PIC X(04).
      *������ô��θ��ۡ��Ƚ�(�������� �����Ƚ�)
               05  SC15-GENMEN OCCURS 2    PIC X(02).
      *������ô���ʬ
               05  SC15-ITBFTNKBN OCCURS 8 PIC X(02).
