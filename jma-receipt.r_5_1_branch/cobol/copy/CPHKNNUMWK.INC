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
      * redistribute this program, but only under the conditions desc
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    �ݸ��ֹ�ޥ��������ǡ���  (CPHKNNUMWK.INC)            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  HKNWK-REC.
      *������ʬ
          03  HKNWK-SYORIKBN             PIC X(01).
      *����
          03  HKNWK-KEY.
      *���ŵ��أɣ�
               05 HKNWK-HOSPID            PIC  X(24).
      *�ݸ��ֹ�
               05 HKNWK-HKNNUM            PIC  X(03).
      *Ŭ�ѳ���ǯ����
               05 HKNWK-TEKSTYMD          PIC  X(08).
      *��ʧ��ʬ
               05 HKNWK-PAYKBN            PIC  X(02).
      *Ŭ�ѽ�λǯ����
           03 HKNWK-TEKEDYMD              PIC  X(08).
      *Ŭ�ѽ�λǯ������
           03 HKNWK-TEKEDYMD2             PIC  X(08).
      *ˡ���ֹ�
           03 HKNWK-HBTNUM                PIC  X(02).
      *����̾
           03 HKNWK-SEIDONAME             PIC  X(100).
      *û������̾
           03 HKNWK-TANSEIDONAME          PIC  X(20).
      *�ݸ�������̶�ʬ
           03 HKNWK-HKNKOHSBTKBN          PIC  X(01).
      *ˡ���ֹ�����å���ʬ
           03 HKNWK-HBTNUMCHKKBN          PIC  X(01).
      *�����ֹ�����å���ʬ
           03 HKNWK-KENSNUMCHKKBN         PIC  X(01).
      *����Ը����ֹ�����å���ʬ
           03 HKNWK-JKYSKENSNUMCHKKBN     PIC  X(01).
      *������ݶ�ʬ
           03 HKNWK-HKNKOHKBN             PIC  X(01).
      *�����ݸ��ֹ�
           03 HKNWK-GENHKNNUM-G.
      *�����ݸ��ֹ棱
               05 HKNWK-GENHKNNUM1        PIC  X(03).
      *�����ݸ��ֹ棲
               05 HKNWK-GENHKNNUM2        PIC  X(03).
      *�����ݸ��ֹ棳
               05 HKNWK-GENHKNNUM3        PIC  X(03).
      *
           03 HKNWK-GENHKNNUM-R       REDEFINES   HKNWK-GENHKNNUM-G.
               05 HKNWK-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *����ǯ�𳫻�
           03 HKNWK-JOKEN-STYEAR          PIC  9(03).
      *����ǯ��λ
           03 HKNWK-JOKEN-EDYEAR          PIC  9(03).
      *����ñ��
           03 HKNWK-TENTNK                PIC  9(03).
      *�쥻�ץ������ʬ
           03 HKNWK-RECESKYKBN            PIC  X(01).
      *�쥻�ץȣ���ñ��ɽ����ʬ
           03 HKNWK-RECEFTNDSPKBN         PIC  X(01).
      *�쥻�ץȴ�����ô��Ƚ��ɽ����ʬ
           03 HKNWK-RECEJGNDSPKBN         PIC  X(01).
      *
           03 HKNWK-NAIYOU.
      *�ܿ�
              05 HKNWK-HON.
      *  �ܿ͡ݳ������ô��ʬ
                07 HKNWK-HON-GAIFTNKBN         PIC  X(01).
      *  �ܿ͡ݳ��������
                07 HKNWK-HON-GAI-NAIYO.
      *    �ܿ͡ݳ���ݲ����ô���
                  09 HKNWK-HON-GAIKFTNRATE         PIC  9(03).
      *    �ܿ͡ݳ���ݲ�ݸ����
                  09 HKNWK-HON-GAIKKOTGAK          PIC  9(06).
      *    �ܿ͡ݳ���ݲ�ݾ�³�
                  09 HKNWK-HON-GAIKJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�³�
                  09 HKNWK-HON-GAIDJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�²��
                  09 HKNWK-HON-GAIDJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWK-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWK-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݾ�²��
                  09 HKNWK-HON-GAIMJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ����������ô��ʬ
                  09 HKNWK-HON-GAIYKZFTNKBN        PIC  X(01).
      *  �ܿ͡���������ô��ʬ
                07 HKNWK-HON-NYUFTNKGN             PIC  X(01).
      *  �ܿ͡�����������
                07 HKNWK-HON-NYU-NAIYO.
      *    �ܿ͡������ݲ����ô���
                  09 HKNWK-HON-NYUKFTNRATE         PIC  9(03).
      *    �ܿ͡������ݲ�ݸ����
                  09 HKNWK-HON-NYUKKOTGAK          PIC  9(06).
      *    �ܿ͡������ݲ�ݾ�³�
                  09 HKNWK-HON-NYUKJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�³�
                  09 HKNWK-HON-NYUDJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�²��
                  09 HKNWK-HON-NYUDGJNCNT          PIC  9(02).
      *    �ܿ͡������ݷ�ݾ�³�
                  09 HKNWK-HON-NYUMJGNGAK          PIC  9(06).
      *    �ܿ͡������ݷ�ݾ�²��
                  09 HKNWK-HON-NYUMJGNCNT          PIC  9(02).
      *    �ܿ͡������ݿ������ܶ�ʬ
                  09 HKNWK-HON-NYUSKJRYYKBN        PIC  X(01).
      *    �ܿ͡������ݿ�����ô�����ۡ�����
                  09 HKNWK-HON-NYUSKJJYOGAK        PIC  9(06).
      *��²
              05 HKNWK-KZK.
      *  ��²�ݳ������ô��ʬ
                07 HKNWK-KZK-GAIFTNKBN           PIC  X(01).
      *  ��²�ݳ��������
                07 HKNWK-KZK-GAI-NAIYO.
      *    ��²�ݳ���ݲ����ô���
                  09 HKNWK-KZK-GAIKFTNRATE         PIC  9(03).
      *    ��²�ݳ���ݲ�ݸ����
                  09 HKNWK-KZK-GAIKKOTGAK          PIC  9(06).
      *    ��²�ݳ���ݲ�ݾ�³�
                  09 HKNWK-KZK-GAIKJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�³�
                  09 HKNWK-KZK-GAIDJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�²��
                  09 HKNWK-KZK-GAIDJGNCNT          PIC  9(02).
      *    ��²�ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWK-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWK-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݾ�²��
                  09 HKNWK-KZK-GAIMJGNCNT          PIC  9(02).
      *    ��²�ݳ����������ô��ʬ
                  09 HKNWK-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  ��²����������ô��ʬ
                07 HKNWK-KZK-NYUFTNKBN             PIC  X(01).
      *  ��²������������
                07 HKNWK-KZK-NYU-NAIYO.
      *    ��²�������ݲ����ô���
                  09 HKNWK-KZK-NYUKFTNRATE         PIC  9(03).
      *    ��²�������ݲ�ݸ����
                  09 HKNWK-KZK-NYUKKOTGAK          PIC  9(06).
      *    ��²�������ݲ�ݾ�³�
                  09 HKNWK-KZK-NYUKJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�³�
                  09 HKNWK-KZK-NYUDJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�²��
                  09 HKNWK-KZK-NYUDJGNCNT          PIC  9(02).
      *    ��²�������ݷ�ݾ�³�
                  09 HKNWK-KZK-NYUMJGNGAK          PIC  9(06).
      *    ��²�������ݷ�ݾ�²��
                  09 HKNWK-KZK-NYUMJGNCNT          PIC  9(02).
      *    ��²�������ݿ������ܶ�ʬ
                  09 HKNWK-KZK-NYUSKJRYYKBN        PIC  X(01).
      *    ��²�������ݿ�����ô�����ۡ�����
                  09 HKNWK-KZK-NYUSKJJYOGAK        PIC  9(06).
      *�����
              05 HKNWK-TSY.
      *  ������ݳ������ô��ʬ
                07 HKNWK-TSY-GAIFTNKBN         PIC  X(01).
      *  ������ݳ��������
                07 HKNWK-TSY-GAI-NAIYO.
      *    ������ݳ���ݲ����ô���
                  09 HKNWK-TSY-GAIKFTNRATE      PIC  9(03).
      *    ������ݳ���ݲ�ݸ����
                  09 HKNWK-TSY-GAIKKOTGAK       PIC  9(06).
      *    ������ݳ���ݲ�ݾ�³�
                  09 HKNWK-TSY-GAIKJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�³�
                  09 HKNWK-TSY-GAIDJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�²��
                  09 HKNWK-TSY-GAIDJGNCNT       PIC  9(02).
      *    ������ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWK-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWK-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݾ�²��
                  09 HKNWK-TSY-GAIMJGNCNT       PIC  9(02).
      *    ������ݳ����������ô��ʬ
                  09 HKNWK-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  ���������������ô��ʬ
                07 HKNWK-TSY-NYUFTNNBK          PIC  X(01).
      *  �����������������
                07 HKNWK-TSY-NYU-NAIYO.
      *    ������������ݲ����ô���
                  09 HKNWK-TSY-NYUKFTNRATE      PIC  9(03).
      *    ������������ݲ�ݸ����
                  09 HKNWK-TSY-NYUKKOTGAK       PIC  9(06).
      *    ������������ݲ�ݾ�³�
                  09 HKNWK-TSY-NYUKJGNGAK       PIC  9(06).
      *    ����������������ݾ�³�
                  09 HKNWK-TSY-NYUDJGNGAK       PIC  9(06).
      *    ����������������ݾ�²��
                  09 HKNWK-TSY-NYUDJGNCNT       PIC  9(02).
      *    ������������ݷ�ݾ�³�
                  09 HKNWK-TSY-NYUMJGNGAK       PIC  9(06).
      *    ������������ݷ�ݾ�²��
                  09 HKNWK-TSY-NYUMJGNCNT       PIC  9(02).
      *    ������������ݿ������ܶ�ʬ
                  09 HKNWK-TSY-NYUSKJRYYKBN     PIC  X(01).
      *    ������������ݿ�����ô�����ۡ�����
                  09 HKNWK-TSY-NYUSKJJYOGAK     PIC  9(06).
      *��ǯ��
              05 HKNWK-TNK.
      *  ��ǯ��ݳ������ô��ʬ
                07 HKNWK-TNK-GAIFTNKBN         PIC  X(01).
      *  ��ǯ��ݳ���������
                07 HKNWK-TNK-GAI-NAIYO.
      *    ��ǯ��ݳ���ݲ����ô���
                  09 HKNWK-TNK-GAIKFTNRATE      PIC  9(03).
      *    ��ǯ��ݳ���ݲ�ݸ����
                  09 HKNWK-TNK-GAIKKOTGAK       PIC  9(06).
      *    ��ǯ��ݳ���ݲ�ݾ�³�
                  09 HKNWK-TNK-GAIKJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�³�
                  09 HKNWK-TNK-GAIDJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�²��
                  09 HKNWK-TNK-GAIDJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWK-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWK-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݾ�²��
                  09 HKNWK-TNK-GAIMJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ����������ô��ʬ
                  09 HKNWK-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  ��ǯ�����������ô��ʬ
                07 HKNWK-TNK-NYUFTNKBN        PIC  X(01).
      *  ��ǯ�������������
                07 HKNWK-TNK-NYU-NAIYO.
      *    ��ǯ��������ݲ����ô���
                  09 HKNWK-TNK-NYUKFTNRATE      PIC  9(03).
      *    ��ǯ��������ݲ�ݸ����
                  09 HKNWK-TNK-NYUKKOTGAK       PIC  9(06).
      *    ��ǯ��������ݲ�ݾ�³�
                  09 HKNWK-TNK-NYUKJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�³�
                  09 HKNWK-TNK-NYUDJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�²��
                  09 HKNWK-TNK-NYUDJGNCNT       PIC  9(02).
      *    ��ǯ��������ݷ�ݾ�³�
                  09 HKNWK-TNK-NYUMJGNGAK       PIC  9(06).
      *    ��ǯ��������ݷ�ݾ�²��
                  09 HKNWK-TNK-NYUMJGNCNT       PIC  9(02).
      *    ��ǯ��������ݿ������ܶ�ʬ
                  09 HKNWK-TNK-NYUSKJRYYKBN     PIC  X(01).
      *    ��ǯ��������ݿ�����ô�����ۡ�����
                  09 HKNWK-TNK-NYUSKJJYOGAK     PIC  9(06).
      *
           03 HKNWK-NAIYOU-R                REDEFINES   HKNWK-NAIYOU.
              05 HKNWK-TBL                  OCCURS  4.
      *  �������ô��ʬ
                07 HKNWK-GAIFTNKBN          PIC  X(01).
      *  ���������
                07 HKNWK-GAI-NAIYO.
      *    ����ݲ����ô���
                  09 HKNWK-GAIKFTNRATE         PIC  9(03).
      *    ����ݲ�ݸ����
                  09 HKNWK-GAIKKOTGAK          PIC  9(06).
      *    ����ݲ�ݾ�³�
                  09 HKNWK-GAIKJGNGAK          PIC  9(06).
      *    ��������ݾ�³�
                  09 HKNWK-GAIDJGNGAK          PIC  9(06).
      *    ��������ݾ�²��
                  09 HKNWK-GAIDJGNCNT          PIC  9(02).
      *    ����ݷ�ݱ���ݾ�³�
                  09 HKNWK-GAIMNAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݱ����ݾ�³�
                  09 HKNWK-GAIMGAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݾ�²��
                  09 HKNWK-GAIMJGNCNT          PIC  9(02).
      *    �����������ô��ʬ
                  09 HKNWK-GAIYKZFTNKBN        PIC  X(01).
      *  ��������ô��ʬ
                07 HKNWK-NYUFTNKBN             PIC  X(01).
      *  ����������
                07 HKNWK-NYU-NAIYO.
      *    �����ݲ����ô���
                  09 HKNWK-NYUKFTNRATE         PIC  9(03).
      *    �����ݲ�ݸ����
                  09 HKNWK-NYUKKOTGAK          PIC  9(06).
      *    �����ݲ�ݾ�³�
                  09 HKNWK-NYUKJGNGAK          PIC  9(06).
      *    ���������ݾ�³�
                  09 HKNWK-NYUDJGNGAK          PIC  9(06).
      *    ���������ݾ�²��
                  09 HKNWK-NYUDJGNCNT          PIC  9(02).
      *    �����ݷ�ݾ�³�
                  09 HKNWK-NYUMJGNGAK          PIC  9(06).
      *    �����ݷ�ݾ�²��
                  09 HKNWK-NYUMJGNCNT          PIC  9(02).
      *    �����ݿ������ܶ�ʬ
                  09 HKNWK-NYUSKJRYYKBN        PIC  X(01).
      *    �����ݿ�����ô�����ۡ�����
                  09 HKNWK-NYUSKJJYOGAK        PIC  9(06).
      *����ü���ɣ�
           03 HKNWK-TERMID                  PIC  X(16).
      *�������ڡ��졼���ɣ�
           03 HKNWK-OPID                    PIC  X(16).
      *��������ǯ����
           03 HKNWK-CREYMD                  PIC  X(08).
      *��������ǯ����
           03 HKNWK-UPYMD                   PIC  X(08).
      *������������
           03 HKNWK-UPHMS                   PIC  X(06).
