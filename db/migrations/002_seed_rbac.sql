-- ==============================================================================
-- 002 : données de référence (rôles, permissions, paramètres). Aucun secret.
-- Le premier super administrateur est créé en Phase C par un script dédié,
-- avec un mot de passe saisi au moment de l'initialisation.
-- ==============================================================================

INSERT INTO permissions (code, description) VALUES
  ('dashboard.read',         'Consulter le tableau de bord'),
  ('reports.read',           'Consulter les rapports et analyses'),
  ('users.read',             'Consulter les utilisateurs'),
  ('users.manage',           'Créer, modifier, désactiver des utilisateurs'),
  ('roles.manage',           'Gérer les rôles et leurs permissions'),
  ('customers.read',         'Consulter les clients'),
  ('customers.write',        'Créer et modifier les clients'),
  ('customers.export',       'Exporter les données clients'),
  ('subscriptions.read',     'Consulter les abonnements'),
  ('subscriptions.write',    'Créer et modifier les abonnements'),
  ('predictions.run',        'Lancer une prédiction individuelle'),
  ('predictions.batch',      'Lancer des prédictions par lot'),
  ('predictions.read',       'Consulter l''historique des prédictions'),
  ('campaigns.read',         'Consulter les campagnes de rétention'),
  ('campaigns.write',        'Créer et modifier les campagnes'),
  ('campaigns.update_own',   'Mettre à jour le suivi de ses propres dossiers'),
  ('model.monitor',          'Superviser les performances du modèle'),
  ('audit.read',             'Consulter les journaux d''audit'),
  ('settings.manage',        'Modifier les paramètres de la plateforme')
ON CONFLICT (code) DO NOTHING;

INSERT INTO roles (code, label, is_system) VALUES
  ('super_admin', 'Super administrateur', true),
  ('admin',       'Administrateur',       true),
  ('data_ai',     'Responsable Data / IA', true),
  ('sales_agent', 'Agent commercial',     true),
  ('auditor',     'Analyste / Auditeur',  true)
ON CONFLICT (code) DO NOTHING;

-- super_admin : toutes les permissions. Autres rôles : liste explicite.
WITH mapping (role_code, perm_code) AS (VALUES
  ('admin','users.read'), ('admin','users.manage'), ('admin','dashboard.read'),
  ('admin','reports.read'), ('admin','customers.read'), ('admin','customers.write'),
  ('admin','customers.export'), ('admin','subscriptions.read'),
  ('admin','subscriptions.write'), ('admin','predictions.run'),
  ('admin','predictions.batch'), ('admin','predictions.read'),
  ('admin','campaigns.read'), ('admin','campaigns.write'), ('admin','model.monitor'),

  ('data_ai','dashboard.read'), ('data_ai','reports.read'), ('data_ai','customers.read'),
  ('data_ai','subscriptions.read'), ('data_ai','predictions.run'),
  ('data_ai','predictions.batch'), ('data_ai','predictions.read'),
  ('data_ai','campaigns.read'), ('data_ai','model.monitor'),

  ('sales_agent','dashboard.read'), ('sales_agent','customers.read'),
  ('sales_agent','subscriptions.read'), ('sales_agent','predictions.read'),
  ('sales_agent','campaigns.read'), ('sales_agent','campaigns.update_own'),

  ('auditor','dashboard.read'), ('auditor','reports.read'), ('auditor','customers.read'),
  ('auditor','subscriptions.read'), ('auditor','predictions.read'),
  ('auditor','campaigns.read'), ('auditor','model.monitor'), ('auditor','audit.read')
)
INSERT INTO role_permissions (role_id, permission_id)
SELECT r.id, p.id
FROM roles r
JOIN permissions p
  ON r.code = 'super_admin'
  OR EXISTS (SELECT 1 FROM mapping m WHERE m.role_code = r.code AND m.perm_code = p.code)
ON CONFLICT DO NOTHING;

INSERT INTO settings (key, value, description) VALUES
  ('risk_thresholds',
   '{"medium": 0.30, "high": 0.60}',
   'Seuils de probabilité de churn : au-dessus de "medium" = risque moyen, au-dessus de "high" = risque élevé'),
  ('auth_policy',
   '{"max_failed_attempts": 5, "lockout_minutes": 15, "session_idle_minutes": 30, "session_max_hours": 8}',
   'Politique de connexion et de session'),
  ('batch_limits',
   '{"max_rows": 5000, "max_file_mb": 5}',
   'Limites des imports CSV')
ON CONFLICT (key) DO NOTHING;
