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
      *    �ݸ��ֹ�          �� COPY (CPHKNNUM.INC)              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HKNNUM-REC.
      *����
           03 HKN-KEY.
      *���ŵ��أɣ�
               05 HKN-HOSPNUM            PIC 9(2).
      *�ݸ��ֹ�
               05 HKN-HKNNUM            PIC  X(03).
      *Ŭ�ѳ���ǯ����
               05 HKN-TEKSTYMD          PIC  X(08).
      *��ʧ��ʬ
               05 HKN-PAYKBN            PIC  X(02).
      *Ŭ�ѽ�λǯ����
           03 HKN-TEKEDYMD              PIC  X(08).
      *ˡ���ֹ�
           03 HKN-HBTNUM                PIC  X(02).
      *����̾
           03 HKN-SEIDONAME             PIC  X(100).
      *û������̾
           03 HKN-TANSEIDONAME          PIC  X(20).
      *�ݸ�������̶�ʬ
           03 HKN-HKNKOHSBTKBN          PIC  X(01).
      *ˡ���ֹ�����å���ʬ
           03 HKN-HBTNUMCHKKBN          PIC  X(01).
      *�����ֹ�����å���ʬ
           03 HKN-KENSNUMCHKKBN         PIC  X(01).
      *����Ը����ֹ�����å���ʬ
           03 HKN-JKYSKENSNUMCHKKBN     PIC  X(01).
      *������ݶ�ʬ
           03 HKN-HKNKOHKBN             PIC  X(01).
      *�����ݸ��ֹ�
           03 HKN-GENHKNNUM-G.
      *�����ݸ��ֹ棱
               05 HKN-GENHKNNUM1        PIC  X(03).
      *�����ݸ��ֹ棲
               05 HKN-GENHKNNUM2        PIC  X(03).
      *�����ݸ��ֹ棳
               05 HKN-GENHKNNUM3        PIC  X(03).
      *
           03 HKN-GENHKNNUM-R           REDEFINES   HKN-GENHKNNUM-G.
               05 HKN-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *����ǯ�𳫻�
           03 HKN-JOKEN-STYEAR          PIC  9(03).
      *����ǯ��λ
           03 HKN-JOKEN-EDYEAR          PIC  9(03).
      *����ñ��
           03 HKN-TENTNK                PIC  9(03).
      *�쥻�ץ������ʬ
           03 HKN-RECESKYKBN            PIC  X(01).
      *�쥻�ץȣ���ñ��ɽ����ʬ
           03 HKN-RECEFTNDSPKBN         PIC  X(01).
      *�쥻�ץȴ�����ô��Ƚ��ɽ����ʬ
           03 HKN-RECEJGNDSPKBN         PIC  X(01).
      *
           03 HKN-NAIYOU.
      *�ܿ�
              05 HKN-HON.
      *  �ܿ͡ݳ������ô��ʬ
                07 HKN-HON-GAIFTNKBN         PIC  X(01).
      *  �ܿ͡ݳ��������
                07 HKN-HON-GAI-NAIYO.
      *    �ܿ͡ݳ���ݲ����ô���
                  09 HKN-HON-GAIKFTNRATE         PIC  9(03).
      *    �ܿ͡ݳ���ݲ�ݸ����
                  09 HKN-HON-GAIKKOTGAK          PIC  9(06).
      *    �ܿ͡ݳ���ݲ�ݾ�³�
                  09 HKN-HON-GAIKJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�³�
                  09 HKN-HON-GAIDJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�²��
                  09 HKN-HON-GAIDJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ���ݷ�ݱ���ݾ�³�
                  09 HKN-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݱ����ݾ�³�
                  09 HKN-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݾ�²��
                  09 HKN-HON-GAIMJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ����������ô��ʬ
                  09 HKN-HON-GAIYKZFTNKBN        PIC  X(01).
      *  �ܿ͡���������ô��ʬ
                07 HKN-HON-NYUFTNKGN             PIC  X(01).
      *  �ܿ͡�����������
                07 HKN-HON-NYU-NAIYO.
      *    �ܿ͡������ݲ����ô���
                  09 HKN-HON-NYUKFTNRATE         PIC  9(03).
      *    �ܿ͡������ݲ�ݸ����
                  09 HKN-HON-NYUKKOTGAK          PIC  9(06).
      *    �ܿ͡������ݲ�ݾ�³�
                  09 HKN-HON-NYUKJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�³�
                  09 HKN-HON-NYUDJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�²��
                  09 HKN-HON-NYUDGJNCNT          PIC  9(02).
      *    �ܿ͡������ݷ�ݾ�³�
                  09 HKN-HON-NYUMJGNGAK          PIC  9(06).
      *    �ܿ͡������ݷ�ݾ�²��
                  09 HKN-HON-NYUMJGNCNT          PIC  9(02).
      *    �ܿ͡������ݿ������ܶ�ʬ
                  09 HKN-HON-NYUSKJRYYKBN        PIC  X(01).
      *    �ܿ͡������ݿ�����ô�����ۡ�����
                  09 HKN-HON-NYUSKJJYOGAK        PIC  9(06).
      *��²
              05 HKN-KZK.
      *  ��²�ݳ������ô��ʬ
                07 HKN-KZK-GAIFTNKBN           PIC  X(01).
      *  ��²�ݳ��������
                07 HKN-KZK-GAI-NAIYO.
      *    ��²�ݳ���ݲ����ô���
                  09 HKN-KZK-GAIKFTNRATE         PIC  9(03).
      *    ��²�ݳ���ݲ�ݸ����
                  09 HKN-KZK-GAIKKOTGAK          PIC  9(06).
      *    ��²�ݳ���ݲ�ݾ�³�
                  09 HKN-KZK-GAIKJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�³�
                  09 HKN-KZK-GAIDJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�²��
                  09 HKN-KZK-GAIDJGNCNT          PIC  9(02).
      *    ��²�ݳ���ݷ�ݱ���ݾ�³�
                  09 HKN-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݱ����ݾ�³�
                  09 HKN-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݾ�²��
                  09 HKN-KZK-GAIMJGNCNT          PIC  9(02).
      *    ��²�ݳ����������ô��ʬ
                  09 HKN-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  ��²����������ô��ʬ
                07 HKN-KZK-NYUFTNKBN             PIC  X(01).
      *  ��²������������
                07 HKN-KZK-NYU-NAIYO.
      *    ��²�������ݲ����ô���
                  09 HKN-KZK-NYUKFTNRATE         PIC  9(03).
      *    ��²�������ݲ�ݸ����
                  09 HKN-KZK-NYUKKOTGAK          PIC  9(06).
      *    ��²�������ݲ�ݾ�³�
                  09 HKN-KZK-NYUKJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�³�
                  09 HKN-KZK-NYUDJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�²��
                  09 HKN-KZK-NYUDJGNCNT          PIC  9(02).
      *    ��²�������ݷ�ݾ�³�
                  09 HKN-KZK-NYUMJGNGAK          PIC  9(06).
      *    ��²�������ݷ�ݾ�²��
                  09 HKN-KZK-NYUMJGNCNT          PIC  9(02).
      *    ��²�������ݿ������ܶ�ʬ
                  09 HKN-KZK-NYUSKJRYYKBN        PIC  X(01).
      *    ��²�������ݿ�����ô�����ۡ�����
                  09 HKN-KZK-NYUSKJJYOGAK        PIC  9(06).
      *�����
              05 HKN-TSY.
      *  ������ݳ������ô��ʬ
                07 HKN-TSY-GAIFTNKBN         PIC  X(01).
      *  ������ݳ��������
                07 HKN-TSY-GAI-NAIYO.
      *    ������ݳ���ݲ����ô���
                  09 HKN-TSY-GAIKFTNRATE      PIC  9(03).
      *    ������ݳ���ݲ�ݸ����
                  09 HKN-TSY-GAIKKOTGAK       PIC  9(06).
      *    ������ݳ���ݲ�ݾ�³�
                  09 HKN-TSY-GAIKJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�³�
                  09 HKN-TSY-GAIDJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�²��
                  09 HKN-TSY-GAIDJGNCNT       PIC  9(02).
      *    ������ݳ���ݷ�ݱ���ݾ�³�
                  09 HKN-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݱ����ݾ�³�
                  09 HKN-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݾ�²��
                  09 HKN-TSY-GAIMJGNCNT       PIC  9(02).
      *    ������ݳ����������ô��ʬ
                  09 HKN-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  ���������������ô��ʬ
                07 HKN-TSY-NYUFTNNBK          PIC  X(01).
      *  �����������������
                07 HKN-TSY-NYU-NAIYO.
      *    ������������ݲ����ô���
                  09 HKN-TSY-NYUKFTNRATE      PIC  9(03).
      *    ������������ݲ�ݸ����
                  09 HKN-TSY-NYUKKOTGAK       PIC  9(06).
      *    ������������ݲ�ݾ�³�
                  09 HKN-TSY-NYUKJGNGAK       PIC  9(06).
      *    ����������������ݾ�³�
                  09 HKN-TSY-NYUDJGNGAK       PIC  9(06).
      *    ����������������ݾ�²��
                  09 HKN-TSY-NYUDJGNCNT       PIC  9(02).
      *    ������������ݷ�ݾ�³�
                  09 HKN-TSY-NYUMJGNGAK       PIC  9(06).
      *    ������������ݷ�ݾ�²��
                  09 HKN-TSY-NYUMJGNCNT       PIC  9(02).
      *    ������������ݿ������ܶ�ʬ
                  09 HKN-TSY-NYUSKJRYYKBN     PIC  X(01).
      *    ������������ݿ�����ô�����ۡ�����
                  09 HKN-TSY-NYUSKJJYOGAK     PIC  9(06).
      *��ǯ��
              05 HKN-TNK.
      *  ��ǯ��ݳ������ô��ʬ
                07 HKN-TNK-GAIFTNKBN         PIC  X(01).
      *  ��ǯ��ݳ���������
                07 HKN-TNK-GAI-NAIYO.
      *    ��ǯ��ݳ���ݲ����ô���
                  09 HKN-TNK-GAIKFTNRATE      PIC  9(03).
      *    ��ǯ��ݳ���ݲ�ݸ����
                  09 HKN-TNK-GAIKKOTGAK       PIC  9(06).
      *    ��ǯ��ݳ���ݲ�ݾ�³�
                  09 HKN-TNK-GAIKJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�³�
                  09 HKN-TNK-GAIDJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�²��
                  09 HKN-TNK-GAIDJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ���ݷ�ݱ���ݾ�³�
                  09 HKN-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݱ����ݾ�³�
                  09 HKN-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݾ�²��
                  09 HKN-TNK-GAIMJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ����������ô��ʬ
                  09 HKN-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  ��ǯ�����������ô��ʬ
                07 HKN-TNK-NYUFTNKBN        PIC  X(01).
      *  ��ǯ�������������
                07 HKN-TNK-NYU-NAIYO.
      *    ��ǯ��������ݲ����ô���
                  09 HKN-TNK-NYUKFTNRATE      PIC  9(03).
      *    ��ǯ��������ݲ�ݸ����
                  09 HKN-TNK-NYUKKOTGAK       PIC  9(06).
      *    ��ǯ��������ݲ�ݾ�³�
                  09 HKN-TNK-NYUKJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�³�
                  09 HKN-TNK-NYUDJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�²��
                  09 HKN-TNK-NYUDJGNCNT       PIC  9(02).
      *    ��ǯ��������ݷ�ݾ�³�
                  09 HKN-TNK-NYUMJGNGAK       PIC  9(06).
      *    ��ǯ��������ݷ�ݾ�²��
                  09 HKN-TNK-NYUMJGNCNT       PIC  9(02).
      *    ��ǯ��������ݿ������ܶ�ʬ
                  09 HKN-TNK-NYUSKJRYYKBN     PIC  X(01).
      *    ��ǯ��������ݿ�����ô�����ۡ�����
                  09 HKN-TNK-NYUSKJJYOGAK     PIC  9(06).
      *
           03 HKN-NAIYOU-R                REDEFINES   HKN-NAIYOU.
              05 HKN-TBL                  OCCURS  4.
      *  �������ô��ʬ
                07 HKN-GAIFTNKBN          PIC  X(01).
      *  ���������
                07 HKN-GAI-NAIYO.
      *    ����ݲ����ô���
                  09 HKN-GAIKFTNRATE         PIC  9(03).
      *    ����ݲ�ݸ����
                  09 HKN-GAIKKOTGAK          PIC  9(06).
      *    ����ݲ�ݾ�³�
                  09 HKN-GAIKJGNGAK          PIC  9(06).
      *    ��������ݾ�³�
                  09 HKN-GAIDJGNGAK          PIC  9(06).
      *    ��������ݾ�²��
                  09 HKN-GAIDJGNCNT          PIC  9(02).
      *    ����ݷ�ݱ���ݾ�³�
                  09 HKN-GAIMNAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݱ����ݾ�³�
                  09 HKN-GAIMGAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݾ�²��
                  09 HKN-GAIMJGNCNT          PIC  9(02).
      *    �����������ô��ʬ
                  09 HKN-GAIYKZFTNKBN        PIC  X(01).
      *  ��������ô��ʬ
                07 HKN-NYUFTNKBN             PIC  X(01).
      *  ����������
                07 HKN-NYU-NAIYO.
      *    �����ݲ����ô���
                  09 HKN-NYUKFTNRATE         PIC  9(03).
      *    �����ݲ�ݸ����
                  09 HKN-NYUKKOTGAK          PIC  9(06).
      *    �����ݲ�ݾ�³�
                  09 HKN-NYUKJGNGAK          PIC  9(06).
      *    ���������ݾ�³�
                  09 HKN-NYUDJGNGAK          PIC  9(06).
      *    ���������ݾ�²��
                  09 HKN-NYUDJGNCNT          PIC  9(02).
      *    �����ݷ�ݾ�³�
                  09 HKN-NYUMJGNGAK          PIC  9(06).
      *    �����ݷ�ݾ�²��
                  09 HKN-NYUMJGNCNT          PIC  9(02).
      *    �����ݿ������ܶ�ʬ
                  09 HKN-NYUSKJRYYKBN        PIC  X(01).
      *    �����ݿ�����ô�����ۡ�����
                  09 HKN-NYUSKJJYOGAK        PIC  9(06).
      *����ü���ɣ�
           03 HKN-TERMID                  PIC  X(16).
      *�������ڡ��졼���ɣ�
           03 HKN-OPID                    PIC  X(16).
      *��������ǯ����
           03 HKN-CREYMD                  PIC  X(08).
      *��������ǯ����
           03 HKN-UPYMD                   PIC  X(08).
      *������������
           03 HKN-UPHMS                   PIC  X(06).
