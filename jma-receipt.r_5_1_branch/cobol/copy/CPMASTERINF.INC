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
      *    �ޥ��������о�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  MASTERINF-AREA.
           03  MASTERINF-VAL.
      *
               05 PIC X(50) VALUE  "tbl_adrs".
               05 PIC X(50) VALUE  "����".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_byomei".
               05 PIC X(50) VALUE  "��̾".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_chk".
               05 PIC X(50) VALUE  "�����å�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_chksnd".
               05 PIC X(50) VALUE  "�����å���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_etensu_1".
               05 PIC X(50) VALUE  "�Ż�����ɽ��".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_2_jma".
               05 PIC X(50) VALUE  "�Ż�����ɽ���ɲ�".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_2_sample".
               05 PIC X(50) VALUE  "�Ż�����ɽ������".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_2".
               05 PIC X(50) VALUE  "�Ż�����ɽ��".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_1".
               05 PIC X(50) VALUE  "�Ż�����ɽ���ݣ�".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_2".
               05 PIC X(50) VALUE  "�Ż�����ɽ���ݣ�".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_3".
               05 PIC X(50) VALUE  "�Ż�����ɽ���ݣ�".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_4".
               05 PIC X(50) VALUE  "�Ż�����ɽ���ݣ�".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_4".
               05 PIC X(50) VALUE  "�Ż�����ɽ��".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_5".
               05 PIC X(50) VALUE  "�Ż�����ɽ��".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_example".
               05 PIC X(50) VALUE  "����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_generic_class".
               05 PIC X(50) VALUE  "������ʬ��".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_generic_price".
               05 PIC X(50) VALUE  "��������".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_genericname".
               05 PIC X(50) VALUE  "����̾".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hknjainf_care".
               05 PIC X(50) VALUE  "����ݸ��Ծ���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hknjainf".
               05 PIC X(50) VALUE  "�ݸ��Ծ���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hknnum".
               05 PIC X(50) VALUE  "�ݸ��ֹ�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hktsantei".
               05 PIC X(50) VALUE  "�������å�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hktsrycd".
               05 PIC X(50) VALUE  "�����ť�����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_infection_judgment".
               05 PIC X(50) VALUE  "������Ƚ��".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_interact".
               05 PIC X(50) VALUE  "��ߺ���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_kensasort".
               05 PIC X(50) VALUE  "����ʬ��".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_labor_sio".
               05 PIC X(50) VALUE  "ϫƯ�����Ľ�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_material_h".
               05 PIC X(50) VALUE  "����������".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_material_k".
               05 PIC X(50) VALUE  "�ú����ܻ����Ϣ�굻".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_material_s".
               05 PIC X(50) VALUE  "�ú����ܻ���Ŭ������".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuincdchg".
               05 PIC X(50) VALUE  "�������������ִ�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuinkhn".
               05 PIC X(50) VALUE  "����������".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuinryakusho".
               05 PIC X(50) VALUE  "�����쥻�ץȵ���ά��".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuksnchk".
               05 PIC X(50) VALUE  "�������û������å�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_prgoption".
               05 PIC X(50) VALUE  "�ץ�����४�ץ����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_psychotropic".
               05 PIC X(50) VALUE  "����������ʬ������".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_recekisai".
               05 PIC X(50) VALUE  "�쥻�ץȵ��ܻ���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_senteicdchg".
               05 PIC X(50) VALUE  "���������񡦰��̥����ɿ���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_srycdchg".
               05 PIC X(50) VALUE  "����Ϸ���ִ�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_sskijyo".
               05 PIC X(50) VALUE  "�ɾ����ֵ���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_synonym".
               05 PIC X(50) VALUE  "Ʊ����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_syskanri".
               05 PIC X(50) VALUE  "�����ƥ����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tekiou_disease".
               05 PIC X(50) VALUE  "�����ʽ���̾�ģ�Ŭ������̾���".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tekiou_medicine".
               05 PIC X(50) VALUE  "�����ʽ���̾�ģ°�����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tekioubyomei".
               05 PIC X(50) VALUE  "Ŭ����̾".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tensuplus".
               05 PIC X(50) VALUE  "�����ղ�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tensuplus2".
               05 PIC X(50) VALUE  "�����ղã�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tensu".
               05 PIC X(50) VALUE  "����".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_toukeimemo".
               05 PIC X(50) VALUE  "���׳н�".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_yakujyo".
               05 PIC X(50) VALUE  "���޾���".
               05 PIC X(01) VALUE  "1".
      *
           03  MASTERINF-R  REDEFINES  MASTERINF-VAL.
               05  MASTERINF-OCC   OCCURS    50.
                   07  MASTERINF-ID    PIC X(50).
                   07  MASTERINF-NAME  PIC X(50).
                   07  MASTERINF-EXISTS-UPYMD
                                       PIC X(1).
           03  MASTERINF-MAX       PIC 9(03)
                                   VALUE     50.
