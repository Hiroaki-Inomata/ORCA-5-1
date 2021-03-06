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
      *ɽ��ǯ��
              03  SC07-PRTYM            PIC  X(16).
      *��Ψ�����
              03  SC07-RITU             PIC  X(02).
              03  SC07-GAKU             PIC  X(02).
      *���ֹ�
              03  SC07-KENNUM           PIC  X(02).
      *���ŵ��إ�����
              03  SC07-HOSPCDN          PIC  X(20).
      *����ǯ����
              03  SC07-SEIYMD           PIC  X(22).
      *����
              03  SC07-ADRS             PIC  X(100).
      *���ŵ���̾
              03  SC07-HOSPNAME         PIC  X(100).
      *������̾
              03  SC07-KAISETUNAME      PIC  X(60).
      *�����ֹ�
              03  SC07-TEL              PIC  X(15).
      *���ťơ��֥�
              03  SC07-SRYTBL           OCCURS  10.
      *������ֹ�
                  05  SC07-JKYSNUM      PIC  X(06).
      *���ǼԻ�̾
                  05  SC07-NAME         PIC  X(20).
      *������ʬ
                  05  SC07-NYUGAIKBN-TBL  OCCURS  2.
                      07  SC07-NYUGAIKBN  PIC  X(02).
      *�ݸ���ʬ�����ݡ�����
                  05  SC07-SYAHO        PIC  X(02).
                  05  SC07-KOKUHO       PIC  X(02).
                  05  SC07-TAI          PIC  X(02).
      *����
                  05  SC07-TENSU7       PIC  X(07).
                  05  SC07-KOHTENSU7    PIC  X(06).
                  05  SC07-TENSU8       PIC  X(07).
                  05  SC07-KOHTENSU8    PIC  X(06).
      *���ް�����ô��
                  05  SC07-YKZFTN       PIC  X(07).
      *������
                  05  SC07-NISSU        PIC  X(02).
      *������ô��
                  05  SC07-ITBFTN       PIC  X(07).
      *���ỻ���
                  05  SC07-SEIKYU       PIC  X(09).
      *��ץơ��֥�
              03  SC07-SUM-TBL  OCCURS  2.
      *��ס��������������������
                  05  SC07-GKENSU7      PIC  X(02).
                  05  SC07-GTENSU7      PIC  X(07).
                  05  SC07-GKOHTEN7     PIC  X(07).
                  05  SC07-GKENSU8      PIC  X(02).
                  05  SC07-GTENSU8      PIC  X(07).
                  05  SC07-GKOHTEN8     PIC  X(07).
      *��ס����ް�����ô��
                  05  SC07-GYKZFTN      PIC  X(07).
      *��ס�������
                  05  SC07-GNISSU       PIC  X(03).
      *��ס�������ô��
                  05  SC07-GITBFTN      PIC  X(07).
      *��ס����ỻ���
                  05  SC07-GSEIKYU      PIC  X(09).
