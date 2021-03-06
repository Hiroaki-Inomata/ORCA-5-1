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
           03  SC13-SRYYM                   PIC  X(16).
      *���Ŷ�ʬ
           03  SC13-SRYKBN1                 PIC  X(02).
           03  SC13-SRYKBN2                 PIC  X(02).
      *���ڡ�����
           03  SC13-GPAGE                   PIC  X(02).
      *�ڡ�����
           03  SC13-PAGE                    PIC  X(02).
      *����ǯ����
           03  SC13-SEIYMD                  PIC  X(22).
      *���ŵ��إ�����
           03  SC13-HOSPCD                  PIC  X(20).
      *���ŵ��ؽ����
           03  SC13-ADRS                    PIC  X(100).
      *���ŵ���̾��
           03  SC13-HOSPNAME                PIC  X(100).
      *���߼�̾
           03  SC13-KAISETUNAME             PIC  X(40).
      *�����ֹ�
           03  SC13-TEL                     PIC  X(15).
      *����ơ��֥�
           03  SC13-TBL   OCCURS  12.
      *���ٶ�ʬ
               05  SC13-SEIDOKBN-TBL  OCCURS  5.
                   07  SC13-SEIDOKBN        PIC  X(02).
      *�ݸ����ֹ�
               05  SC13-HKNJANUM            PIC  X(12).
      *�ܿͲ�²��ʬ
               05  SC13-HONKZKKBN           PIC  X(02).
      *����������
               05  SC13-NYUGAIKBN-TBL  OCCURS  2.
      *���
                   07  SC13-KENSU           PIC  X(05).
      *����
                   07  SC13-TENSU           PIC  X(09).
      *����ʬ����
                   07  SC13-KOHTENSU        PIC  X(09).
      *���ް�����ô��
                   07  SC13-YKZFTN          PIC  X(07).
      *Ϸ�Ͱ�����ô�����Ϸ򹯿Ǻ������
                   07  SC13-ITBFTN          PIC  X(09).
      *���������񡧷��
               05  SC13-SKENSU           PIC  X(05).
      *���������񡧶��
               05  SC13-SKINGK           PIC  X(09).
      *����������ɸ����ô��
               05  SC13-SFTNGK           PIC  X(09).
      *������׷��
           03  SC13-N-GKENSU                PIC  X(05).
      *�����׷��
           03  SC13-G-GKENSU                PIC  X(05).
      *���
           03  SC13-GOKEI-TBL    OCCURS  2.
      *����������ơ��֥�
               05  SC13-G-NYUGAI-TBL  OCCURS  2.
      *���
                   07  SC13-GKENSU          PIC  X(05).
      *���
                   07  SC13-GTENSU          PIC  X(09).
