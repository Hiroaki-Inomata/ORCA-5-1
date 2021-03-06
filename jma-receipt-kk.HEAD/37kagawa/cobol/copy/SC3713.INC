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
       01  SC13.
      *����ǯ��
           03  SC13-PRTYM                     PIC  X(16).
      *����ǯ����
           03  SC13-SEIYMD                    PIC  X(22).
      *����ǯ����
           03  SC13-SHOMEIYMD                 PIC  X(22).
      *��Į¼̾
           03  SC13-CITYNAME                  PIC  X(22).
      *�����Ի�̾
           03  SC13-SEINAME                   PIC  X(20).
      *���ŵ��ؽ���
           03  SC13-HOSPADRS                  PIC  X(100).
      *���ŵ���̾��
           03  SC13-HOSPNAME                  PIC  X(100).
      *���߼Ի�̾
           03  SC13-KAISETU                   PIC  X(40).
      *����Ի�ʾ��ֹ�
           03  SC13-JKYSNUM                   PIC  X(14).
      *����Խ���
           03  SC13-ADRS                      PIC  X(80).
      *����Ի�̾
           03  SC13-NAME                      PIC  X(30).
      *��ǯ����
           03  SC13-TANJOYMD                  PIC  X(22).
      *���ݸ��Ի�̾
           03  SC13-HI-NAME                   PIC  X(30).
      *�����ʬ
           03  SC13-KOHKBN                    PIC  X(02) OCCURS 3.
      *����ơ��֥�
           03  SC13-SEIKYU-TBL.
      *�ݸ�����
               05  SC13-HKNSYUBETU    OCCURS 6 PIC X(02).
      *�ݸ���ʬ(���̡��࿦)
               05  SC13-HKNKBN        OCCURS 2 PIC X(02).
      *�����ǡ���
               05  SC13-NYUIN-TBL.
      *�����ݿ���������
                   07  SC13-N-TENSU.
                       09  SC13-N-TEN           OCCURS 5 PIC X(02).
      *������¾ˡ��ô��
                   07  SC13-N-THFTNGK.
                       09  SC13-N-THFTN         OCCURS 5 PIC X(02).
      *�������ݸ�����ô����
                   07  SC13-N-HKNJAFTNGK.
                       09  SC13-N-HKNJAFTN      OCCURS 5 PIC X(02).
      *�����ݰ�����ô��(�ܿ���ô��)
                   07  SC13-N-ITBFTN       PIC X(18).
      *����ɸ����ô
                   07  SC13-N-HYJFTN       PIC X(18).
      *��������ǯ����
                   07  SC13-N-NYUINYMD     PIC X(22).
      *�����ݷ�³
                   07  SC13-KEIZOKU        PIC X(02).
      *�������̱�����
                   07  SC13-N-NISSU        PIC X(04).
      *����ǡ���
               05  SC13-GAIRAI-TBL.
      *����ݿ���������
                   07  SC13-G-TENSU.
                       09  SC13-G-TEN           OCCURS 5 PIC X(02).
      *�����¾ˡ��ô��
                   07  SC13-G-THFTNGK.
                       09  SC13-G-THFTN         OCCURS 5 PIC X(02).
      *������ݸ�����ô����
                   07  SC13-G-HKNJAFTNGK.
                       09  SC13-G-HKNJAFTN      OCCURS 5 PIC X(02).
      *����ݰ�����ô��(�ܿ���ô��)
                   07  SC13-G-ITBFTN       PIC  X(18).
      *��������ް�����ô��
                   07  SC13-G-YKZFTN       PIC  X(18).
