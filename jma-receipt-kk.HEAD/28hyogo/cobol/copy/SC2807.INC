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
        01  SC07.
      *����ǯ��
            03  SC07-PRTYM                PIC X(16).
      *����ǯ����
            03  SC07-SEIYMD               PIC X(22).
      *��Į¼̾
            03  SC07-CITYNAME             PIC X(20).
      *���ŵ��ؽ���
            03  SC07-ADRS                 PIC X(100).
      *���ŵ���̾
            03  SC07-HOSPNAME             PIC X(100).
      *���߼�̾
            03  SC07-KAISETU              PIC X(40).
      *���ŵ��إ�����
            03  SC07-HOSPCD               PIC X(14).
      *��ô���
            03  SC07-FTNWARI
                                          PIC X(02)    OCCURS  3 TIMES.
      *���٥ơ��֥�
            03  SC07-MEISAI-TBL           OCCURS  20 TIMES.
      *Ĺ��
                05  SC07-TYOKI1           PIC X(02).
                05  SC07-TYOKI2           PIC X(02).
      *��̾
                05  SC07-NAME             PIC X(24).
      *����ǯ��
                05  SC07-TANJOYM          PIC X(10).
      *��ô���ֹ�
                05  SC07-FTNJANUM         PIC X(08).
      *������ֹ�
                05  SC07-JKYSNUM          PIC X(14).
      *����ǯ��
                05  SC07-SRYYM            PIC X(08).
      *������ʬ
                05  SC07-NYUGAIKBN
                                          PIC X(02)    OCCURS  2 TIMES.
      *����
                05  SC07-NISSU            PIC X(04).
      *����
                05  SC07-TENSU            PIC X(12).
      *������ô��(Ϸ��)
                05  SC07-RITBFTN          PIC X(12).
      *������ô��(�����)
                05  SC07-ITBFTN           PIC X(10).
      *����
                05  SC07-BIKO1            PIC X(02).
                05  SC07-BIKO2            PIC X(02).
      *��ץơ��֥�
            03  SC07-GOKEI-TBL.
      *��׷��
                05  SC07-GKENSU           PIC X(04).
      *�������
                05  SC07-GTENSU           PIC X(14).
      *��װ�����ô��(Ϸ��)
                05  SC07-GRITBFTN         PIC X(14).
      *��װ�����ô��(�����)
                05  SC07-GITBFTN          PIC X(14).
