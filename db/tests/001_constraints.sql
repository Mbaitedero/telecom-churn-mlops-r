-- Tests des contraintes de la migration 001. Tout est annulé (ROLLBACK) à la fin.
BEGIN;

DO $$
DECLARE
  sa_role    smallint;
  admin_role smallint;
  u1 uuid;
  u2 uuid;
  u3 uuid;
  others     integer;
BEGIN
  SELECT id INTO sa_role    FROM roles WHERE code = 'super_admin';
  SELECT id INTO admin_role FROM roles WHERE code = 'admin';
  IF sa_role IS NULL OR admin_role IS NULL THEN
    RAISE EXCEPTION 'ECHEC : rôles de référence absents (seed 002 manquant)';
  END IF;

  SELECT count(*) INTO others FROM users WHERE role_id = sa_role AND status = 'active';

  INSERT INTO users (username, email, full_name, password_hash, role_id)
  VALUES ('t_sa1', 't_sa1@example.test', 'Test SA 1', 'hash', sa_role)
  RETURNING id INTO u1;

  -- 1) Le dernier super administrateur actif est protégé (seulement si aucun autre n'existe)
  IF others = 0 THEN
    BEGIN
      UPDATE users SET status = 'disabled', disabled_at = now() WHERE id = u1;
      RAISE EXCEPTION 'ECHEC : le dernier super administrateur a pu être désactivé';
    EXCEPTION WHEN integrity_constraint_violation THEN
      RAISE NOTICE 'OK 1 : le dernier super administrateur est protégé';
    END;
  ELSE
    RAISE NOTICE 'IGNORE 1 : d''autres super administrateurs existent déjà';
  END IF;

  -- 2) Avec un second super administrateur, la désactivation du premier est autorisée
  INSERT INTO users (username, email, full_name, password_hash, role_id)
  VALUES ('t_sa2', 't_sa2@example.test', 'Test SA 2', 'hash', sa_role)
  RETURNING id INTO u2;
  UPDATE users SET status = 'disabled', disabled_at = now() WHERE id = u1;
  RAISE NOTICE 'OK 2 : désactivation autorisée quand un autre super administrateur existe';

  -- 3) Unicité insensible à la casse du nom d'utilisateur
  BEGIN
    INSERT INTO users (username, email, full_name, password_hash, role_id)
    VALUES ('T_SA2', 'autre@example.test', 'Doublon', 'hash', admin_role);
    RAISE EXCEPTION 'ECHEC : doublon de nom d''utilisateur accepté';
  EXCEPTION WHEN unique_violation THEN
    RAISE NOTICE 'OK 3 : nom d''utilisateur unique sans tenir compte de la casse';
  END;

  -- 4) Un compte désactivé doit avoir une date de désactivation
  INSERT INTO users (username, email, full_name, password_hash, role_id)
  VALUES ('t_adm1', 't_adm1@example.test', 'Test Admin', 'hash', admin_role)
  RETURNING id INTO u3;
  BEGIN
    UPDATE users SET status = 'disabled' WHERE id = u3;
    RAISE EXCEPTION 'ECHEC : compte désactivé sans date';
  EXCEPTION WHEN check_violation THEN
    RAISE NOTICE 'OK 4 : cohérence statut / date de désactivation';
  END;

  -- 5) Le journal d'audit est en ajout seul
  INSERT INTO audit_log (action, result) VALUES ('test.sql', 'success');
  BEGIN
    UPDATE audit_log SET action = 'modifie' WHERE action = 'test.sql';
    RAISE EXCEPTION 'ECHEC : audit_log modifiable';
  EXCEPTION WHEN insufficient_privilege THEN
    RAISE NOTICE 'OK 5a : modification de audit_log refusée';
  END;
  BEGIN
    DELETE FROM audit_log WHERE action = 'test.sql';
    RAISE EXCEPTION 'ECHEC : audit_log supprimable';
  EXCEPTION WHEN insufficient_privilege THEN
    RAISE NOTICE 'OK 5b : suppression dans audit_log refusée';
  END;

  RAISE NOTICE 'Tests de la migration 001 terminés sans échec';
END $$;

ROLLBACK;
