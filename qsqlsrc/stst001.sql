          EXEC SQL
             DECLARE C1 CURSOR FOR
             SELECT MAX(CDINDICE) FROM FTBLP010
             FOR FETCH ONLY;

          EXEC SQL OPEN C1;
          EXEC SQL CLOSE C1;
