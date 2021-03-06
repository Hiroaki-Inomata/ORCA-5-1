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
      *    ��������                    COPY  (CPDASCONF.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  DASCF-REC.
           03  DASCF-KEY.
      *���ŵ��ؼ����ֹ�
               05  DASCF-HOSPNUM             PIC  9(02).
      *������
           03  DASCF-SINSEIYMD               PIC  X(08).
      *��̾�ʥ������ʡ�
           03  DASCF-KANANAME                PIC  X(50).
      *��̾
           03  DASCF-NAME                    PIC  X(50).
      *�᡼�륢�ɥ쥹
           03  DASCF-EMAIL                   PIC  X(256).
      *���ŵ��ؼ��̣�
           03  DASCF-HOSPSBT1                PIC  X(01).
      *���ŵ��ؼ��̣�
           03  DASCF-HOSPSBT2                PIC  X(01).
      *�¾����ʰ��̡�
           03  DASCF-BEDSUIPPAN              PIC  9(04).
      *�¾��������ܡ�
           03  DASCF-BEDSURYOYO              PIC  9(04).
      *�¾�����������
           03  DASCF-BEDSUSEISIN             PIC  9(04).
      *�¾����ʷ�ˡ�
           03  DASCF-BEDSUKEKKAKU            PIC  9(04).
      *�¾��������Ρ�
           03  DASCF-BEDSU                   PIC  9(04).
      *���ּ���
           03  DASCF-SETTI                   PIC  X(01).
      *�礿����Ų�
           03  DASCF-SYUKA                   PIC  X(02).
      *�ϰ���Żٱ��±�
           03  DASCF-CHIIKI                  PIC  X(01).
      *�ģУ��о��±�
           03  DASCF-DPCKBN                  PIC  X(01).
      *����ٱ���Ž�
           03  DASCF-ZAITAKU                 PIC  X(01).
      *��������
           03  DASCF-KAIINYM                 PIC  X(06).
      *͹���ֹ�
           03  DASCF-POST                    PIC  X(07).
      *���������ֹ�
           03  DASCF-USERID                  PIC  X(15).
      *�ѥ����
           03  DASCF-PASSWORD                PIC  X(128).
      *Ʊ��
           03  DASCF-AGREEMENT               PIC  X(01).
      *�������
           03  DASCF-SENDRESULT              PIC  X(03).
      *������
           03  DASCF-SENDDATE                PIC  X(08).
      *ü���ɣ�
           03  DASCF-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  DASCF-OPID                    PIC  X(16).
      *����ǯ����
           03  DASCF-CREYMD                  PIC  X(08).
      *����ǯ����
           03  DASCF-UPYMD                   PIC  X(08).
      *��������
           03  DASCF-UPHMS                   PIC  X(06).
