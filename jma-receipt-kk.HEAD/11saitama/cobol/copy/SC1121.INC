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
       01  SC21.
      *�����Ե������ꥢ(Ģɼ����)
           03  SC21-SINSEI-AREA.
      *��������
               05  SC21-SEIYMD1                    PIC  X(22).
      *�����Խ���
               05  SC21-SINSEIADRS                 PIC  X(80).
      *�����Ի�̾
               05  SC21-SINSEINAME                 PIC  X(40).
      *�����������ֹ�
               05  SC21-SINSEITEL                  PIC  X(15).
      *������ֹ�
               05  SC21-JKYSNUM                    PIC  X(14).
      *���ʻ�̾
               05  SC21-KANANAME                   PIC  X(30).
      *��̾
               05  SC21-NAME                       PIC  X(20).
      *��ǯ����
               05  SC21-TANJOYY                    PIC  X(02).
               05  SC21-TANJOMM                    PIC  X(02).
               05  SC21-TANJODD                    PIC  X(02).
      *�ݸ����ֹ�
               05  SC21-HKNJANUM                   PIC  X(16).
      *�ݸ���̾
               05  SC21-HKNJANAME                  PIC  X(20).
      *���ݸ���̾
               05  SC21-HIHKNJANAME                PIC  X(20).
      *���ǡ������
               05  SC21-KAZEI1                     PIC  X(02).
               05  SC21-KAZEI2                     PIC  X(02).
      *���ŵ��ص������ꥢ(Ģɼ�����)
           03  SC21-HOSP-AREA.
      *���Ų�̾
               05  SC21-SRYKANAME                  PIC  X(20).
      *�ݸ����Ű�����ô��(������)
               05  SC21-SEIKYUGK                   PIC  X(09).
      *����ǯ��
               05  SC21-SRYYY                      PIC  X(04).
               05  SC21-SRYMM                      PIC  X(04).
      *������ʬ
               05  SC21-NYUGAIKBN1                 PIC  X(02).
               05  SC21-NYUGAIKBN2                 PIC  X(02).
      *������
               05  SC21-NISSU                      PIC  X(02).
      *���ŵ��إ�����
               05  SC21-HOSPCD                     PIC  X(14).
      *������
               05  SC21-TENSU                      PIC  X(12).
      *¾ˡʬ����
               05  SC21-TAHOU                      PIC  X(12).
      *ɸ����ô��
               05  SC21-FTNGK                      PIC  X(06).
      *��������
               05  SC21-SNISSU                     PIC  X(02).
      *��������
               05  SC21-SEIYMD2                    PIC  X(22).
      *������̾��
               05  SC21-SINSEINAME2                PIC  X(30).
      *���ŵ��ؽ���
               05  SC21-HOSPADRS                   PIC  X(100).
      *���ŵ���̾��
               05  SC21-HOSPNAME                   PIC  X(100).
      *���߼�̾
               05  SC21-KAISETU                    PIC  X(40).
      *��(��)
           03  SC21-MOJI                           PIC  X(02).
